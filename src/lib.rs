#![feature(bigint_helper_methods)]
#![allow(non_snake_case)]
mod utils;
mod opcodes;

use crate::opcodes::init_opcodes;
use crate::opcodes::Opcodes;

use std::collections::{VecDeque};
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(name : &str) {
    alert(& format!("Hello, {}!", name));
}
const ROM: &[u8; 32768] = include_bytes!("../tests/Hello World.sms");
#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct Size{pub width : usize, pub height : usize}

#[wasm_bindgen]
pub struct Emulator{
    buffers : VecDeque<Vec<u8>>,
    running : bool,

    opcodes : Opcodes,
    state : EmulatorState,

    time : f64,
    pub size : Size,
}

#[wasm_bindgen]
impl Emulator{
    fn new(width : usize, height : usize, opcodes : Opcodes) -> Self{

        let mut buffers = VecDeque::new();
        buffers.push_front(vec![0u8; width * height * 4]);
        buffers.push_front(vec![0u8; width * height * 4]);
        buffers.push_front(vec![0u8; width * height * 4]);
        buffers[1][1] = 100;
        buffers[2][1] = 200;
        let size = Size{width, height};
        let time = 0.0;
        let running = false;
        let state = EmulatorState::new();
        Self{buffers, size, running, time, opcodes, state}
    }
    pub fn start(&mut self) {
        self.running = true;
    }
    pub fn clock(&mut self) -> u16 {
        let op = &self.opcodes[self.state.next()];
        op(&mut self.state);
        self.state.PC
    }

    pub fn mem_around_pc(&mut self, buffer : &mut[u8]) -> usize {
        let size = buffer.len();
        let halfsize = buffer.len() / 2;
        let pos = self.state.PC as usize;

        let start = if pos < halfsize {0}else{pos-halfsize};
        for i in 0..size {
            buffer[i] = self.state.mem_read((start + i) as u16);
        }
        pos
    }

    pub fn pause(&mut self){
        self.running = false;
    }
    
    pub fn get_state(&mut self) -> EmulatorState{
        self.state
    }
    pub fn get_flags(&mut self) -> EmulatorFlags{
        self.state.flags()
    }
    
    pub fn tick(&mut self, time : f64) -> f64{
        let delta = time - self.time;
        println!("delta {:?}", delta);
        if self.running {
            self.clock();
        }
        if self.running && delta >= 1000./60.{
            let mut buf = self.take();
            self.time = time;
            let value = 0.5 + (self.time / 1000.0).sin() * 0.5;
            for y in 0..self.size.height{
                let row = y * self.size.width;
                for x in 0..self.size.width{
                    let idx = (row + x) * 4;
                    let ratio = idx as f64 / (self.size.width * self.size.height * 4) as f64;
                    let value = if ratio > value { 0 }else { 128 };
                    buf[idx + 0] = value;
                    buf[idx + 1] = value;
                    buf[idx + 2] = value;
                    buf[idx + 3] = 255u8;
                }
            }
            self.buffers.push_back(buf.to_vec());
            delta
        }else{
            println!("too small");
            -delta
        }
    }
    
    pub fn take(&mut self) -> Vec<u8>{
        self.buffers.pop_back().expect("ran out of buffers")
    }
    
    pub fn swap(&mut self, give : Vec<u8>) -> Vec<u8>{
        self.buffers.push_front(give);
        self.take()
    }

}
#[repr(C)]
#[derive(Clone, Copy)]
#[wasm_bindgen]
pub struct EmulatorState {
    ram : [u8; 0x2000],
    ports : [u8; 0xFF],
    cart_ram : [[u8; 0x2000]; 2],
    pub A : u8, //ACC
    pub F : u8, //flags
    pub BC : u16,// General purpose registers
    pub DE : u16,
    pub HL : u16,

    pub IX : u16,//Index registers
    pub IY : u16,
    pub PC : u16,//Program counter
    pub SP : u16,//Stack pointer

    pub I : u8,//Interrupt data register
    pub R : u8,//Refresh/random register

    pub AFp : u16,// shadow registers
    pub BCp : u16,
    pub DEp : u16,
    pub HLp : u16,
    mappers : [u8; 4],
    pub interrupt_mode : u8,
    pub next_offset : Option<i8>,
    pub interrupts_enabled : bool
}

impl EmulatorState{
     #[allow(non_snake_case)]
    fn new() -> Self {
    let ram : [u8; 0x2000] = [0; 0x2000];
        let ports : [u8; 0xFF] = [0; 0xFF];
        let cart_ram : [[u8; 0x2000]; 2] = [[0; 0x2000]; 2];
        let A : u8 = 0;
        let F : u8 = 0;
        let BC : u16 = 0;
        let DE : u16 = 0;
        let HL : u16 = 0;

        let IX : u16 = 0;
        let IY : u16 = 0;
        let PC : u16 = 0;
        let SP : u16 = 0;

        let I : u8 = 0;
        let R : u8 = 0;

        let AFp : u16 = 0;
        let BCp : u16 = 0;
        let DEp : u16 = 0;
        let HLp : u16 = 0;
        let mappers : [u8; 4] = [0, 0, 0, 0];
        let interrupts_enabled : bool = true;
        let interrupt_mode = 0;
        let next_offset = None;

        let mut ret = Self{
            ram, ports, cart_ram, A, F, BC, DE, HL,
            IX, IY, PC, SP, I, R,
            AFp, BCp, DEp, HLp, mappers,
            interrupts_enabled, interrupt_mode, next_offset
        };
        ret.reset();
        ret
    }

    fn reset(&mut self){
        self.PC = 0;
        self.A = 0xff;
        self.F = 0xff;
        self.SP = 0xffff;
    }
    fn noop(&mut self) -> usize { 0 }

    #[allow(non_snake_case)]
    pub fn flags(&self ) -> EmulatorFlags {
        let f = self.F;
        let SF = (f & 0b1 << 0) != 0;
        let ZF = (f & 0b1 << 1) != 0;
        let YF = (f & 0b1 << 2) != 0;
        let HF = (f & 0b1 << 3) != 0;
        let XF = (f & 0b1 << 4) != 0;
        let PF = (f & 0b1 << 5) != 0;
        let NF = (f & 0b1 << 6) != 0;
        let CF = (f & 0b1 << 7) != 0;
        EmulatorFlags{SF,ZF,YF,HF,XF,PF,NF,CF}
    }
    fn flags_set_to(&mut self, flags : EmulatorFlags, value : bool){
        if value {
            self.flags_set(flags);
        }else{
            self.flags_unset(flags);
        }
    }
    /**
     * sets any flag that is 1 on the given flags
     * */
    #[allow(non_snake_case)]
    fn flags_set(&mut self, flags : EmulatorFlags) {
        self.F |= flags.as_mask();
    }
    #[allow(non_snake_case)]
    fn flags_unset(&mut self, flags : EmulatorFlags ) {
        self.F &= !flags.as_mask();
    }
    #[allow(non_snake_case)]
    fn flags_toggle(&mut self, flags : EmulatorFlags ) {
        self.F ^= flags.as_mask();
    }

    fn mem_read16(&self, addr : u16) -> u16 {
        to_u16(self.mem_read(addr), self.mem_read(addr+1))
    }

    fn mem_read(&self, addr : u16) -> u8 {
        let a = addr as usize;
        match addr{
            0xFFFC => self.mappers[0], //info about page 2 (ROM/RAM)
            0xFFFD => self.mappers[1], //position of ROM page 0
            0xFFFE => self.mappers[2], //position of ROM page 1
            0xFFFF => self.mappers[3], //position of ROM page 2
            x if x < 0x400 => ROM[a],//First 1k of ROM Bank 0, never paged out with rest of Page 0
            x if x < 0x4000 => ROM[a+self.mem_read(0xFFFD) as usize],//15k ROM Page 0
            x if x < 0x8000 => ROM[a+self.mem_read(0xFFFE) as usize],//16k ROM Page 1
            x if x < 0xC000 => 
                match (self.mappers[0] >> 2) & 0b11 {//bits 2 & 3
                    0b00 | 0b01 => ROM[a+self.mem_read(0xFFFF) as usize],//16k ROM Page 2
                    0b10 => self.cart_ram[0][a+self.mem_read(0xFFFF) as usize],//16k ROM Page 2
                    0b11 => self.cart_ram[1][a+self.mem_read(0xFFFF) as usize],//16k ROM Page 2
                    b => panic!("{:b} unexpected mapping bit pattern", b)
                }
            x if x < 0xE000 => self.ram[a-0xC000],//8k of on-board RAM
            x if x < 0xFFFC => self.ram[a-0xE000],//Mirror of RAM at $C000-$DFFF
            _ => panic!("{:X} is outside of memory range", a)
        }
    }

    fn mem_write16(&mut self, addr : u16, val : u16) {

        let mut high_byte = 0u8;
        let mut low_byte  = 0u8;

        split_u16(val, &mut high_byte, &mut low_byte);

        self.mem_write(addr, high_byte);
        self.mem_write(addr+1, low_byte);
    }

    fn mem_write(&mut self, addr : u16, val : u8) {
        let a = addr as usize;
        match addr{
            0xFFFC => self.mappers[0] = val, //info about page 2 (ROM/RAM)
            0xFFFD => self.mappers[1] = val, //position of ROM page 0
            0xFFFE => self.mappers[2] = val, //position of ROM page 1
            0xFFFF => self.mappers[3] = val, //position of ROM page 2
            x if x < 0x8000 => panic!("forbidden write at {:X}", a),
            x if x < 0xC000 => 
                match (self.mappers[0] >> 2) & 0b11 {//bits 2 & 3
                    0b00 | 0b01 => panic!("forbidden write at {:X}", a),//16k ROM Page 2
                    0b10 => self.cart_ram[0][a+self.mem_read(0xFFFF) as usize] = val,//16k ROM Page 2
                    0b11 => self.cart_ram[1][a+self.mem_read(0xFFFF) as usize] = val,//16k ROM Page 2
                    b => panic!("{:b} unexpected mapping bit pattern", b)
                }
            x if x < 0xE000 => self.ram[a-0xC000] = val,//8k of on-board RAM
            x if x < 0xFFFC => self.ram[a-0xE000] = val,//Mirror of RAM at $C000-$DFFF
            _ => panic!("{:X} is outside of memory range", a)
        };
    }
    
    fn port_write(&mut self, addr : u8, v : u8) {
        self.ports[addr as usize] = v
    }
    fn port_read(&self, addr : u8) -> u8 {
        self.ports[addr as usize]
    }
    fn dec_b_is_zero(&mut self) -> bool {
        let mut val = 0;
        let mut lb = 0;
        split_u16(self.BC, &mut val, &mut lb);
        val -= 1;
        self.BC = to_u16(val, lb);
        val == 0
    }
    fn daa(&mut self) {
        let least_sig_dgt = self.A & 0x0F;
        let is_decimal = least_sig_dgt < 0xa;// not a decimal
        if !is_decimal {
            self.flags_set(EmulatorFlags::H);
            self.A += 0x06;
        }
        let most_sig_dgt = self.A & 0xF0 >> 4;
        let is_decimal = most_sig_dgt < 0xa;// not a decimal
        if !is_decimal {
            self.flags_set(EmulatorFlags::C);
            self.A += 0x60;
        }
        self.flags_set_to(EmulatorFlags::S, (self.A & 0x80) == 0);
        self.flags_set_to(EmulatorFlags::P, (self.A & 0x01) == 0);
        self.flags_set_to(EmulatorFlags::Z, self.A == 0);
    }
    fn exx(&mut self) {
        let tmp = self.BCp;
        self.BCp = self.BC;
        self.BC = tmp;

        let tmp = self.DEp;
        self.DEp = self.DE;
        self.DE = tmp;

        let tmp = self.HLp;
        self.HLp = self.HL;
        self.HL = tmp;
    }

    fn next(&mut self) -> u8{
        let byte = self.mem_read(self.PC);
        self.PC += 1;
        byte
    }

    fn o(&mut self) -> i8{
        self.next() as i8
    }

    fn nn(&mut self) -> u16{
        to_u16(self.next(), self.next())
    }

    fn stack_pop(&mut self) -> u16{
        let val = self.mem_read16(self.SP);
        self.SP += 2;
        val
    }

    fn stack_push(&mut self, v : u16){//TODO: return previous stack value ?
        self.mem_write16(self.SP, v);
        self.SP -= 2;
    }

}

#[wasm_bindgen]
#[derive(Default, Clone, Copy)]
#[allow(non_snake_case)]
pub struct EmulatorFlags{
    pub SF : bool,
    pub ZF : bool,
    pub YF : bool,
    pub HF : bool,
    pub XF : bool,
    pub PF : bool,
    pub NF : bool,
    pub CF : bool,
}

impl EmulatorFlags{
    pub const fn new( SF : bool, ZF : bool, YF : bool, HF : bool,
            XF : bool,PF : bool,NF : bool,CF : bool) -> Self {
        Self{SF,ZF,YF,HF,XF,PF,NF,CF}
    }
    pub fn as_mask(&self) -> u8 {
        let mut msk = 0u8;
        if self.SF{ msk |= 0b1 << 0; }
        if self.ZF{ msk |= 0b1 << 1; }
        if self.YF{ msk |= 0b1 << 2; }
        if self.HF{ msk |= 0b1 << 3; }
        if self.XF{ msk |= 0b1 << 4; }
        if self.PF{ msk |= 0b1 << 5; }
        if self.NF{ msk |= 0b1 << 6; }
        if self.CF{ msk |= 0b1 << 7; }
        msk
    }
    pub const ALL : Self = Self::new(true, true, true, true ,true, true, true, true);
    pub const C : Self = Self::new(false, false, false, false ,false, false, false, true);
    pub const Z : Self = Self::new(false, true, false, false ,false, false, false, true);
    pub const H : Self = Self::new(false, false, false, true ,false, false, false, false);
    pub const N : Self = Self::new(false, false, false, false ,false, false, true, false);
    pub const S : Self = Self::new(true, false, false, false ,false, false, false, false);
    pub const P : Self = Self::new(false, false, false, false ,false, true, false, false);
    pub const HN : Self = Self::new(false, false, false, true ,false, false, true, false);
}
pub fn to_u16(high : u8, low : u8) -> u16 {
    ((high as u16) << 8) | low as u16
}

pub fn split_u16(val : u16, high : &mut u8, low : &mut u8){
    *high = (val >> 8)   as u8;
    *low  = (val & 0xFF) as u8;
}

pub fn is_even_bits(v : u8) -> bool {
    v.count_ones() % 2 == 0
}

pub fn is_even_bits16(v : u16) -> bool {
    v.count_ones() % 2 == 0
}

struct VDP {
    ram : [u8; 0x2000]
}


#[wasm_bindgen]
pub fn init() -> Emulator {
    utils::set_panic_hook();
    let opcodes = init_opcodes();
    Emulator::new(500, 400, opcodes)
}

#[wasm_bindgen]
pub fn version()->usize{
    3
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_set_flags() {
        let mut state = EmulatorState::new();
        assert_eq!(0x00, state.F);
        let flags = state.flags();
        assert_eq!(false, flags.CF);
        let mut up_flags : EmulatorFlags = Default::default();
        up_flags.CF = true;
        up_flags.ZF = true;
        state.flags_set(up_flags);

        assert!(state.flags().CF);
        assert!(state.flags().ZF);

        let mut up_flags : EmulatorFlags = Default::default();
        up_flags.ZF = true;
        state.flags_unset(up_flags);

        assert!(state.flags().CF);
        assert!(!state.flags().ZF);
    }

    #[test]
    fn test_toggle_flags() {
        let mut state = EmulatorState::new();
        let mut up_flags : EmulatorFlags = Default::default();
        up_flags.HF = true;
        up_flags.NF = true;
        state.flags_set(up_flags);

        assert!(state.flags().HF);
        assert!(state.flags().NF);
        assert!(!state.flags().ZF);

        let up_flags = EmulatorFlags::Z;
        state.flags_toggle(up_flags);

        assert!(state.flags().HF);
        assert!(state.flags().NF);
        assert!(state.flags().ZF);

        let up_flags = EmulatorFlags::H;
        state.flags_toggle(up_flags);

        assert!(!state.flags().HF);
        assert!(state.flags().NF);
        assert!(state.flags().ZF);

        let up_flags = EmulatorFlags::new(false, false, false, true, false, false, true, false);
        state.flags_toggle(up_flags);
        assert!(state.flags().HF);
        assert!(!state.flags().NF);
        assert!(state.flags().ZF);
    }
}