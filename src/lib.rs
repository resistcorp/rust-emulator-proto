#![feature(bigint_helper_methods)]
#![feature(fn_traits)]
#![allow(non_snake_case)]
mod utils;
mod opcodes;

use crate::opcodes::init_opcodes;
use crate::opcodes::Opcodes;

use std::collections::{VecDeque};
use wasm_bindgen::prelude::*;
use web_sys::console;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    fn alert(s: &str);
}
const ROM: &[u8; 32768] = include_bytes!("../tests/Hello World.sms");
#[wasm_bindgen]
#[derive(Clone, Copy)]
pub struct Size{pub width : usize, pub height : usize}

pub struct Emulator{
    buffers : VecDeque<Vec<u8>>,
    running : bool,

    ports : Vec<Port>,

    state : EmulatorState,
    vdp : VDP,

    time : f64,
    pub size : Size,
}


#[wasm_bindgen]
pub struct Emu {
    emulator : Emulator,
    opcodes : Opcodes
}

#[wasm_bindgen]
impl Emu{
    pub fn start(&mut self){
        self.emulator.start()
    }
    pub fn mem_around_pc(&mut self, buffer : &mut[u8]) -> usize{
        self.emulator.mem_around_pc(buffer)
    }
    pub fn pause(&mut self){
        self.emulator.pause()
    }
    pub fn get_state(&mut self) -> EmulatorState{
        self.emulator.get_state()
    }
    pub fn get_flags(&mut self) -> EmulatorFlags{
        self.emulator.get_flags()
    }
    pub fn clock(&mut self){
        self.emulator.clock(&self.opcodes);
    }
    pub fn advance(&mut self, time : f64) -> f64{
        self.emulator.advance(time, &self.opcodes)
    }

    pub fn take(&mut self) -> Vec<u8>{
        self.emulator.take()
    }
    
    pub fn swap(&mut self, give : Vec<u8>) -> Vec<u8>{
        self.emulator.buffers.push_front(give);
        self.take()
    }
    pub fn size(&mut self) -> Size {
        self.emulator.size
    }

}

impl Emulator{
    fn new(width : usize, height : usize) -> Self{

        let mut buffers = VecDeque::new();
        buffers.push_front(vec![0u8; width * height * 4]);
        buffers.push_front(vec![0u8; width * height * 4]);
        buffers.push_front(vec![0u8; width * height * 4]);
        buffers[1][1] = 100;
        buffers[2][1] = 200;
        let mut ports = (0..0xFF).map(Port::new).collect::<Vec<_>>();
        let vdp = VDP::new();

        let size = Size{width, height};
        let time = 0.0;
        let running = false;
        let state = EmulatorState::new();
        Self{buffers, size, running, time, state, ports, vdp}
    }
    fn start(&mut self) {
        self.running = true;
    }
    fn clock(&mut self, opcodes : &Opcodes) -> u16 {
        opcodes[self.next()](self);
        self.state.PC
    }

    fn mem_around_pc(&mut self, buffer : &mut[u8]) -> usize {
        let size = buffer.len();
        let halfsize = buffer.len() / 2;
        let pos = self.state.PC as usize;

        let start = if pos < halfsize {0}else{pos-halfsize};
        for i in 0..size {
            buffer[i] = self.mem_read((start + i) as u16);
        }
        pos
    }

    fn pause(&mut self){
        self.running = false;
    }
    
    fn get_state(&mut self) -> EmulatorState{
        self.state
    }
    fn get_flags(&mut self) -> EmulatorFlags{
        self.flags()
    }
    
    fn advance(&mut self, time : f64, opcodes : &Opcodes) -> f64{
        let cycletime = 1f64/4_000f64;
        let delta = time - self.time;
        let cycles = (delta * cycletime) as usize;
        println!("delta {:?}", delta);
        if self.running {
            for _ in 0..cycles{
                self.clock(opcodes);
            }
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
    
    fn take(&mut self) -> Vec<u8>{
        self.buffers.pop_back().expect("ran out of buffers")
    }
    
    fn swap(&mut self, give : Vec<u8>) -> Vec<u8>{
        self.buffers.push_front(give);
        self.take()
    }
    
    fn port_write(&mut self, addr : u8, v : u8) {
        let old = self.ports[addr as usize].write(v);
        self.vdp.on_write(addr, old, v);
    }
    fn port_read(&self, addr : u8) -> u8 {
        let v = self.ports[addr as usize].read();
        self.vdp.on_read(addr, v);
        v
    }
    
    fn noop(&mut self) -> usize { 0 }

    #[allow(non_snake_case)]
    pub fn flags(&self ) -> EmulatorFlags {
        let f = self.state.F;
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
        self.state.F |= flags.as_mask();
    }
    #[allow(non_snake_case)]
    fn flags_unset(&mut self, flags : EmulatorFlags ) {
        self.state.F &= !flags.as_mask();
    }
    #[allow(non_snake_case)]
    fn flags_toggle(&mut self, flags : EmulatorFlags ) {
        self.state.F ^= flags.as_mask();
    }

    fn mem_read16(&self, addr : u16) -> u16 {
        to_u16(self.mem_read(addr), self.mem_read(addr+1))
    }

    fn mem_read(&self, addr : u16) -> u8 {
        let a = addr as usize;
        match addr{
            0xFFFC => self.state.mappers[0], //info about page 2 (ROM/RAM)
            0xFFFD => self.state.mappers[1], //position of ROM page 0
            0xFFFE => self.state.mappers[2], //position of ROM page 1
            0xFFFF => self.state.mappers[3], //position of ROM page 2
            x if x < 0x400 => ROM[a],//First 1k of ROM Bank 0, never paged out with rest of Page 0
            x if x < 0x4000 => ROM[a+self.mem_read(0xFFFD) as usize],//15k ROM Page 0
            x if x < 0x8000 => ROM[a+self.mem_read(0xFFFE) as usize],//16k ROM Page 1
            x if x < 0xC000 => 
                match (self.state.mappers[0] >> 2) & 0b11 {//bits 2 & 3
                    0b00 | 0b01 => ROM[a+self.mem_read(0xFFFF) as usize],//16k ROM Page 2
                    0b10 => self.state.cart_ram[0][a+self.mem_read(0xFFFF) as usize],//16k ROM Page 2
                    0b11 => self.state.cart_ram[1][a+self.mem_read(0xFFFF) as usize],//16k ROM Page 2
                    b => panic!("{:b} unexpected mapping bit pattern", b)
                }
            x if x < 0xE000 => self.state.ram[a-0xC000],//8k of on-board RAM
            x if x < 0xFFFC => self.state.ram[a-0xE000],//Mirror of RAM at $C000-$DFFF
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
            0xFFFC => self.state.mappers[0] = val, //info about page 2 (ROM/RAM)
            0xFFFD => self.state.mappers[1] = val, //position of ROM page 0
            0xFFFE => self.state.mappers[2] = val, //position of ROM page 1
            0xFFFF => self.state.mappers[3] = val, //position of ROM page 2
            x if x < 0x8000 => panic!("forbidden write at {:X}", a),
            x if x < 0xC000 => 
                match (self.state.mappers[0] >> 2) & 0b11 {//bits 2 & 3
                    0b00 | 0b01 => panic!("forbidden write at {:X}", a),//16k ROM Page 2
                    0b10 => self.state.cart_ram[0][a+self.mem_read(0xFFFF) as usize] = val,//16k ROM Page 2
                    0b11 => self.state.cart_ram[1][a+self.mem_read(0xFFFF) as usize] = val,//16k ROM Page 2
                    b => panic!("{:b} unexpected mapping bit pattern", b)
                }
            x if x < 0xE000 => self.state.ram[a-0xC000] = val,//8k of on-board RAM
            x if x < 0xFFFC => self.state.ram[a-0xE000] = val,//Mirror of RAM at $C000-$DFFF
            _ => panic!("{:X} is outside of memory range", a)
        };
    }
    fn dec_b_is_zero(&mut self) -> bool {
        let mut val = 0;
        let mut lb = 0;
        split_u16(self.state.BC, &mut val, &mut lb);
        val -= 1;
        self.state.BC = to_u16(val, lb);
        val == 0
    }
    fn daa(&mut self) {
        let least_sig_dgt = self.state.A & 0x0F;
        let is_decimal = least_sig_dgt < 0xa;// not a decimal
        if !is_decimal {
            self.flags_set(EmulatorFlags::H);
            self.state.A += 0x06;
        }
        let most_sig_dgt = self.state.A & 0xF0 >> 4;
        let is_decimal = most_sig_dgt < 0xa;// not a decimal
        if !is_decimal {
            self.flags_set(EmulatorFlags::C);
            self.state.A += 0x60;
        }
        self.flags_set_to(EmulatorFlags::S, (self.state.A & 0x80) == 0);
        self.flags_set_to(EmulatorFlags::P, (self.state.A & 0x01) == 0);
        self.flags_set_to(EmulatorFlags::Z, self.state.A == 0);
    }
    fn exx(&mut self) {
        let tmp = self.state.BCp;
        self.state.BCp = self.state.BC;
        self.state.BC = tmp;

        let tmp = self.state.DEp;
        self.state.DEp = self.state.DE;
        self.state.DE = tmp;

        let tmp = self.state.HLp;
        self.state.HLp = self.state.HL;
        self.state.HL = tmp;
    }

    fn next(&mut self) -> u8{
        let byte = self.mem_read(self.state.PC);
        self.state.PC += 1;
        byte
    }

    fn o(&mut self) -> i8{
        self.next() as i8
    }

    fn nn(&mut self) -> u16{
        to_u16(self.next(), self.next())
    }

    fn stack_pop(&mut self) -> u16{
        let val = self.mem_read16(self.state.SP);
        self.state.SP += 2;
        val
    }

    fn stack_push(&mut self, v : u16){//TODO: return previous stack value ?
        self.mem_write16(self.state.SP, v);
        self.state.SP -= 2;
    }

}
#[repr(C)]
#[derive(Clone, Copy)]
#[wasm_bindgen]
pub struct EmulatorState {
    ram : [u8; 0x2000],
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
    pub interrupts_enabled : bool,
}

impl EmulatorState{
     #[allow(non_snake_case)]
    fn new() -> Self {
        let ram : [u8; 0x2000] = [0; 0x2000];
        let cart_ram : [[u8; 0x2000]; 2] = [[0; 0x2000]; 2];

        let mappers : [u8; 4] = [0, 0, 0, 0];
        let A = 0;
        let F = 0;
        let BC = 0;
        let DE = 0;
        let HL = 0;

        let IX = 0;
        let IY = 0;
        let PC = 0;
        let SP = 0;

        let I = 0;
        let R = 0;

        let AFp = 0;
        let BCp = 0;
        let DEp = 0;
        let HLp = 0;
        let mappers = [0u8; 4];
        let interrupt_mode = 0u8;
        let next_offset = None;
        let interrupts_enabled = false;

        let mut ret = Self{
            ram, cart_ram,
            A,F,BC,DE,HL,IX,IY,PC,SP,I,R,AFp,BCp,DEp,HLp,
            mappers,interrupt_mode,next_offset,interrupts_enabled
        };
        ret.reset();
        ret
    }
    fn reset(&mut self){
        self.PC = 0;
        self.A = 0xff;
        self.F = 0xff;
        self.SP = 0xffff;
        self.interrupts_enabled = true;
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
    pub const  C : Self = Self::new(false, false, false, false ,false, false, false, true);
    pub const  Z : Self = Self::new(false, true, false, false ,false, false, false, true);
    pub const  H : Self = Self::new(false, false, false, true ,false, false, false, false);
    pub const  N : Self = Self::new(false, false, false, false ,false, false, true, false);
    pub const  S : Self = Self::new(true, false, false, false ,false, false, false, false);
    pub const  P : Self = Self::new(false, false, false, false ,false, true, false, false);
    pub const HN : Self = Self::new(false, false, false, true ,false, false, true, false);
}
pub fn to_u16(high : u8, low : u8) -> u16 {
    let mut ret = 0x0000;
    set_low_byte(&mut ret, low);
    set_high_byte(&mut ret, high);
    ret
}

pub fn split_u16(val : u16, high : &mut u8, low : &mut u8){
    *high = high_byte(val);
    *low  = low_byte(val);
}

pub fn low_byte(val : u16) -> u8 {
    (val >>  8) as u8
}

pub fn high_byte(val : u16) -> u8 {
    (val & 0x00FF) as u8
}

pub fn set_low_byte(val : &mut u16, byte : u8) {
    *val = (byte as u16) << 8 | *val & 0x00FF;
}

pub fn set_high_byte(val : &mut u16, byte : u8) {
    *val = (byte as u16) << 0 | *val & 0xFF00;
}

pub fn is_even_bits(v : u8) -> bool {
    v.count_ones() % 2 == 0
}

pub fn is_even_bits16(v : u16) -> bool {
    v.count_ones() % 2 == 0
}

#[derive(Debug, Clone, Copy)]
enum VDPOperation {VRAMWrite, VRAMRead, CRAMWrite, VDPRegisterWrite}
#[derive(Debug, Clone, Copy)]
enum VDPDisplayMode {GraphicsI,Text,GraphicsII,Multicolor}

impl Default for VDPOperation{
    fn default() -> Self {VDPOperation::VRAMWrite}
}
impl Default for VDPDisplayMode{
    fn default() -> Self {VDPDisplayMode::GraphicsI}
}
/**
 * control port : OxBF
 *  $7E = V counter (read) / SN76489 data (write)
 *  $7F = H counter (read) / SN76489 data (write, mirror)
 *  MSBit                       LSBit
 *  CD1 CD0 A13 A12 A11 A10 A09 A08    Second byte written
 *  A07 A06 A05 A04 A03 A02 A01 A00    First byte written
 * */

const VDP_CONTROL : u8 = 0xBE;


#[derive(Debug)]
struct VDP {
    ram : [u8; 0x2000],
    addr : u16,
    code : VDPOperation,
    h_counter : u16,
    v_counter : u16,
    flag_first_byte : bool,
    display_width : u16,
    display_height : u16,
    flag_int : bool,//frame interrupt pending
    flag_ovr : bool,//sprite overflow
    flag_col : bool,//sprite collision
    //Genesys has extended flags
    display_mode : VDPDisplayMode,
    registers : [u8; 16]
}

impl Default for VDP{
    fn default() -> Self {
        let ram = [0u8; 0x2000];
        let addr = Default::default();
        let code = Default::default();
        let h_counter = Default::default();
        let v_counter = Default::default();
        let flag_first_byte = Default::default();
        let display_width = Default::default();
        let display_height = Default::default();
        let flag_int = Default::default();
        let flag_ovr = Default::default();
        let flag_col = Default::default();
        let display_mode = Default::default();
        let registers = Default::default();
        Self{ram, addr, code,h_counter,v_counter,flag_first_byte,display_width,display_height,flag_int,flag_ovr,flag_col,display_mode,registers}
    }
}

trait PortWriteListener {
    fn on_write(&mut self, port : u8, old_val : u8, new_val : u8);
}
trait PortReadListener {
    fn on_read(&mut self, port : u8, val : u8);
}

impl VDP{
    fn new() -> Self {
        let mut ret : Self = Self::default();
        ret
    }
    fn clock(&mut self){
        self.h_counter+= 1;
        if self.h_counter > self.display_width {
            //todo scanline events $C1, $E1, $F1
            self.v_counter += 1;
            self.h_counter = 0;
        }
        if self.v_counter > self.display_height {
            //todo frame event
            self.v_counter = 0;
        }
    }
    fn write_ctrl(&mut self, _before : u8, after:u8){
        console::log_1(&format!("VDP control port was written to {_before} → {after}").into())
    }
    fn read_ctrl(&mut self, value:u8){
        console::log_1(&format!("VDP control port was read to {value}").into())
    }
}
impl PortReadListener for VDP {
    fn on_read(&mut self, port : u8, val : u8){
        match port {
            VDP_CONTROL => self.read_ctrl(val),
        }
    }
}

impl PortWriteListener for VDP {
    fn on_write(&mut self, port : u8, old_val : u8, new_val : u8){
        match port {
            VDP_CONTROL => self.write_ctrl(old_val, new_val),
        }
    }
}

struct Port{
    value : u8,
    address : u8,
}

impl Port{
    fn new(address : u8) -> Self {
        Self{
            value : 0, address,
        }
    }
    fn read(&self) -> u8 {
        let value = self.value;
        value
    }
    fn write(&mut self, value : u8) -> u8 {
        let before = self.value;
        self.value = value;
        before
    }
}

#[wasm_bindgen]
pub fn create_emulo() -> Emu {
    utils::set_panic_hook();
    let opcodes = init_opcodes();
    let emulator = Emulator::new(500, 400);
    Emu{emulator, opcodes}
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
        let mut emulator = Emulator::new(100, 100);
        assert_eq!(0xFF, emulator.state.F);
        let flags = emulator.flags();
        assert_eq!(true, flags.CF);
        let mut up_flags : EmulatorFlags = Default::default();
        up_flags.CF = true;
        up_flags.ZF = true;
        emulator.flags_set(up_flags);

        assert!(emulator.flags().CF);
        assert!(emulator.flags().ZF);

        let mut up_flags : EmulatorFlags = Default::default();
        up_flags.ZF = true;
        emulator.flags_unset(up_flags);

        assert!(emulator.flags().CF);
        assert!(!emulator.flags().ZF);
    }

    #[test]
    fn test_toggle_flags() {
        let mut emulator = Emulator::new(100, 100);
        let mut up_flags : EmulatorFlags = Default::default();
        up_flags.HF = true;
        up_flags.NF = true;
        emulator.state.F = 0x00;
        emulator.flags_set(up_flags);

        assert!(emulator.flags().HF);
        assert!(emulator.flags().NF);
        assert!(!emulator.flags().ZF);

        let up_flags = EmulatorFlags::Z;
        emulator.flags_toggle(up_flags);

        assert!(emulator.flags().HF);
        assert!(emulator.flags().NF);
        assert!(emulator.flags().ZF);

        let up_flags = EmulatorFlags::H;
        emulator.flags_toggle(up_flags);

        assert!(!emulator.flags().HF);
        assert!(emulator.flags().NF);
        assert!(emulator.flags().ZF);

        let up_flags = EmulatorFlags::new(false, false, false, true, false, false, true, false);
        emulator.flags_toggle(up_flags);
        assert!(emulator.flags().HF);
        assert!(!emulator.flags().NF);
        assert!(emulator.flags().ZF);
    }
}