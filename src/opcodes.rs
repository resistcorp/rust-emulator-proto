use std::collections::HashMap;
use std::ops::{IndexMut, Index};
use web_sys::console;

use std::str::FromStr;
use strum_macros::EnumString;

use crate::*;

type Result<T> = std::result::Result<T, String>;

const OP_DATA: &str = include_str!("instructions.csv");

pub struct Opcodes{
    data : Vec<Operation>
}

pub fn init_opcodes() -> Opcodes {
    let mut opcodes : Opcodes = Opcodes{data : Vec::with_capacity(256)};
    for i in 0..=255 {
        let i = i;
        opcodes.data.push(Box::new(move |_s| panic!("opcode {:X} not implemented", i)));
    }
    let mut defs = Vec::with_capacity(256);
    for def in OP_DATA.lines(){
        let parts : Vec<_> = def.split(";").collect();
        let codes : Vec<_> = parts[0].split_whitespace().collect();
        let size : u16 = parts[1].parse().unwrap();
        assert_eq!(size as usize, codes.len());
        let op = parts[2];
        let cycles : u8 = parts[3].split("/").next().unwrap().parse().unwrap();
        let def = OpcodeDef{op, codes, cycles};
        defs.push(def);
    }
    compute_opcode_table(&mut opcodes, defs, 0);
    opcodes
}
fn compute_opcode_table(opcodes : &mut Opcodes, definitions : Vec<OpcodeDef>, recursion_depth : usize) -> (usize, usize) {
    let mut def_map : HashMap<&str, Vec<OpcodeDef>> = HashMap::new();
    for def in definitions {
        def_map.entry(def.codes[recursion_depth]).or_insert_with(Vec::new).push(def);
    }
    let indent = (0..recursion_depth).map(|_| "..").collect::<String>();
    let mut count_parsed : usize = 0;
    let mut total = 0;
    for (key, def) in def_map {
        let base = u8::from_str_radix(key, 16);

        let start = format!("{:?}", &def[0].codes[..=recursion_depth]);
        if def.len() == 1{
            let def = &def[0];
            if let Ok(base) = base {
                total += 1;
                if let Some(op) = make_op(def, recursion_depth){
                    count_parsed += 1;
                    opcodes[base] = op;
                }else{
                    console::log_1(&format!("{} missed single opcode : {:?} ({})",indent, def.codes, def.op).into());
                }
                
            }else{
                let (todo, parsed) = make_composite(opcodes, key, def, recursion_depth);
                total += todo;
                count_parsed += parsed;
                if todo != parsed{
                    console::log_1(&format!("{} missing some composite opcodes : {:?} ({}) {}/{}",indent, def.codes, def.op, parsed, todo).into());
                }
            }
        }else{
            total += def.len();
            let size = def.len();
            if let Ok(base) = base {
                let mut sub_codes : Opcodes = Opcodes{data : Vec::with_capacity(256)};
                for i in 0..=255 {
                    let i = i;
                    let start = start.clone();
                    sub_codes.data.push(Box::new(move |_s| panic!("opcode {:?} {:X} not implemented", start, i)));
                }
                let is_FDCB = recursion_depth == 1 && (start == "[\"FD\", \"CB\"]" || start == "[\"DD\", \"CB\"]");
                let next_depth = if is_FDCB { recursion_depth+2 } else { recursion_depth+1 };
                if is_FDCB {
                    console::log_1(&format!("{indent}>{start}").into());
                    console::log_1(&format!("{indent}>>> we need to go deeper").into());
                }
                let (todo, parsed) = compute_opcode_table(&mut sub_codes, def, next_depth);
                total += todo;
                count_parsed += parsed;
                opcodes[base] = Box::new(move |s| {
                    if is_FDCB {
                        s.state.next_offset = Some(s.o())
                    }
                    let code = s.next();
                    sub_codes[code](s)
                });
                console::log_1(&format!("{} parsed {} opcodes starting with {}",indent, size, key).into());
            }else{
                console::log_1(&format!("{} sub-table operation {} could not be parsed",indent, key).into());
                console::log_1(&format!("{indent}{indent} {def:?}").into());
            }
        }
    }
    console::log_1(&format!("{} was able to parse {} out of {} (failed {})",indent, count_parsed, total, total - count_parsed).into());
    (total, count_parsed)
}

// === "private" stuff
impl IndexMut<u8> for Opcodes{
    fn index_mut(&mut self, idx: u8) -> &mut <Self as Index<u8>>::Output { 
        &mut self.data[idx as usize]
    }
}
impl Index<u8> for Opcodes{
    type Output = Operation;
    fn index(&self, idx: u8) -> &Self::Output { 
        &self.data[idx as usize]
    }
}

//an operation mutates the state and returns a number of cycles
type Operation = Box<dyn Fn(&mut Emulator) -> u8>;
trait OpRead8 : Fn(&mut Emulator) -> u8 {}
trait OpRead16 : Fn(&mut Emulator) -> u16 {}
trait OpWrite8 : Fn(&mut Emulator, u8) -> () {}
trait OpWrite16 : Fn(&mut Emulator, u16) -> () {}
trait OpMut : Fn(&mut Emulator) -> () {}
impl<T : Fn(&mut Emulator) -> u8>  OpRead8 for T {}
impl<T : Fn(&mut Emulator) -> u16>  OpRead16 for T {}
impl<T : Fn(&mut Emulator, u8) -> ()>  OpWrite8 for T {}
impl<T : Fn(&mut Emulator, u16) -> ()>  OpWrite16 for T {}
impl<T : Fn(&mut Emulator) -> ()>  OpMut for T {}

enum Reader{
    NoneR(String),
    R8(Box<dyn OpRead8>),
    R16(Box<dyn OpRead16>),
    EitherR((Box<dyn OpRead8>, Box<dyn OpRead16>))
}

enum Writer{
    NoneW(String),
    W8(Box<dyn OpWrite8>),
    W16(Box<dyn OpWrite16>),
    EitherW((Box<dyn OpWrite8>, Box<dyn OpWrite16>))
}

impl Reader{
    fn is_8(&self) -> bool{
        use Reader::*;
        match self {
            R8(_) => true,
            R16(_) => false,
            NoneR(_) => false,
            EitherR(..) => true,
        }
    }
    fn is_16(&self) -> bool{
        !self.is_8()
    }//NOTE : Either isn't a 16
    fn to_str(&self) -> &str{
        use Reader::*;
        match self {
            R8(_) => "R8",
            R16(_) => "R16",
            NoneR(_) => "NoneR",
            EitherR(..) => "EitherR",
        }
    }
}
impl Writer{
    fn is_8(&self) -> bool{
        use Writer::*;
        match self {
            W8(_) => true,
            W16(_) => false,
            NoneW(_) => false,
            EitherW(..) => true,
        }
    }
    fn is_16(&self) -> bool{
        !self.is_8()
    }//NOTE : Either isn't a 16
    fn to_str(&self) -> &str{
        use Writer::*;
        match self {
            W8(_) => "W8",
            W16(_) => "W16",
            NoneW(_) => "NoneW",
            EitherW(..) => "EitherW",
        }
    }
}

fn compatible(r : &Reader, w : &Writer) -> bool{
    w.is_8() == r.is_8()
}

#[derive(Debug)]
struct OpcodeDef<'a>{
    op : &'a str,
    codes : Vec<&'a str>,
    cycles : u8
}

use self::Instructions::*;
fn make_standalone(op: Instructions, val : &[&str]) -> Option<Box<dyn OpMut>>{
    // let val : Vec<_> = val.iter().map(|s|s.to_string()).collect();
    use Reader::*;
    use Writer::*;
    match op{
        NOP => Some(Box::new(|_s| {})),
        //Interrupts
        EI => Some(Box::new(|s| {
            s.state.interrupts_enabled = true;
        })),
        DI => Some(Box::new(|s| {
            s.state.interrupts_enabled = false;
        })),
        IM =>{
            assert_eq!(1, val.len());
            let mode = val[0].parse::<u8>().expect("only modes 0-2 exist, and only mode 1 is allowed for Sega MS");

            Some(Box::new(move |s| {
                assert_eq!(1, mode, "Master system only allows IM mode 1");
                s.state.interrupt_mode = mode;
            }))
        },
        RET | RETI | RETN =>{//TODO find difference, depending on interrupt
            assert!(val.len() <= 1);
            let cnd = if val.len() == 1 {
                make_condition(val[0])
            }else{
                make_condition("true")
            };
            Some(Box::new(move |s| {
                if cnd(s) {
                    s.state.PC = s.stack_pop();
                }
            }))
        },
        RST =>{
            assert_eq!(1, val.len());
            let imm = u16::from_str_radix(val[0], 16).expect("RST only exists with an immediate value");
            Some(Box::new(move |s| {
                s.state.SP += 1;
                //this +3 is dodgy to me, should be +1
                s.stack_push(s.state.PC+3);
                s.state.PC = imm;
            }))
        },
        // bitwise
        SET | BIT | RES => {
            assert_eq!(1, val.len());
            make_bitwise(op, val[0])
        },
        // arithmetic
        ADD | ADC | SBC => {
            let pos : Vec<_> = val[0].split(",").collect();
            assert_eq!(1, val.len());
            assert_eq!(2, pos.len());
            let (reader_l, writer) = match pos[0] {
                "A" => (
                    R8(Box::new(|s| s.state.A)),
                    W8(Box::new(|s, v| s.state.A = v))
                ),
                reg => (make_reader(reg), make_writer(reg))
            };
            let reader_r = make_reader(pos[1]);
            use Reader::*;
            use Writer::*;
            match (reader_l, reader_r, writer) {
                (R8(rl), R8(rr), W8(w)) | (R8(rl), EitherR((rr, ..)), W8(w))=> {
                    let arithmetic = match op {
                        ADD => |a : u8, b, _ | a.carrying_add(b, false) ,
                        ADC => |a : u8, b, cf| a.carrying_add(b, cf) ,
                        SBC => |a : u8, b, cf| a.borrowing_sub(b, cf),
                        _ => unreachable!()
                    };
                    Some(Box::new(move |s| {
                                            //Carry case
                                            let carry_was_set = s.flags().CF;
                                            let (v, c) = arithmetic(rl(s), rr(s), carry_was_set);
                                            w(s, v);
                                            s.flags_set_to(EmulatorFlags::C, c);
                                         }))
                },
                (R16(rl), R16(rr), W16(w)) | (EitherR((_, rl)), R16(rr), EitherW((_, w))) => {
                    let arithmetic = match op {
                        ADD => |a : u16, b, _ | a.carrying_add(b, false) ,
                        ADC => |a : u16, b, cf| a.carrying_add(b, cf) ,
                        SBC => |a : u16, b, cf| a.borrowing_sub(b, cf),
                        _ => unreachable!()
                    };
                    Some(Box::new(move |s| {
                                            //Carry case
                                            let carry_was_set = s.flags().CF;
                                            let (v, c) = arithmetic(rl(s), rr(s), carry_was_set);
                                            w(s, v);
                                            s.flags_set_to(EmulatorFlags::C, c);
                                         }))
                },
                _=>None
            }
        },
        OR | AND | XOR | SUB | CP  => {
            assert_eq!(1, val.len());
            let reader = make_reader(&val[0]);
            match reader {
                R8(r) | EitherR((r, ..)) =>{
                    let arithmetic = match op {
                        AND => |a, b| (a & b, false) ,
                        OR => |a, b| (a | b, false),
                        XOR => |a, b| (a ^ b, false),
                        SUB | CP => | a, b| (a - b, false),
                        _ => unreachable!()
                    };
                    Some(Box::new(move |s| {
                                        let (val, _carry) = arithmetic(s.state.A, r(s));
                                        if op != CP {
                                            s.state.A = val;
                                        }
                                        s.flags_set_to(EmulatorFlags::Z, val == 0);
                                        if op == SUB {
                                            s.flags_set(EmulatorFlags::N);
                                        }
                                        //TODO : flags for signed ops which ones when ?
                                        s.flags_set_to(EmulatorFlags::S, (val & 0x80) != 0);
                                        // s.flags_set_to(EmulatorFlags::C, carry);
                                    }))

                },
                _ => panic!("can only {:?} with 8 bit values ({:?})", op, val)
            
}        },

        NEG =>{
            Some(Box::new(|s| {
                let signed = -(s.state.A as i8);

                s.state.A = signed as u8;
                s.flags_set(EmulatorFlags::N);
                s.flags_set_to(EmulatorFlags::Z, signed == 0);
                s.flags_set_to(EmulatorFlags::S, signed < 0);
            }))
        },

        //flag ops
        SCF =>{
            //unset C,
            Some(Box::new(|s| {
                s.flags_set(EmulatorFlags::C);
                s.flags_unset(EmulatorFlags::HN);
            }))
        },
        CPL =>{
            Some(Box::new(|s| {
                s.state.A ^= 0xFF;
                s.flags_set(EmulatorFlags::HN);
            }))
        },
        CCF =>{
            //invert C & H flag,
            //clear N
            Some(Box::new(|s| {
                s.flags_unset(EmulatorFlags::N);
                s.flags_toggle(EmulatorFlags::C);
                s.flags_toggle(EmulatorFlags::H);
            }))
        },
        RLD | RRD => {
            let is_left = op == RLD;
            //Performs a 4-bit leftward rotation of the 12-bit number
            //whose 4 most signigifcant bits are the 4 least significant bits of A,
            //and its 8 least significant bits are in (HL).
            Some(Box::new(move |s| {
                let v = s.mem_read(s.state.HL);
                let h = s.state.A & 0xF;
                let m = v & 0xF0 >> 4;
                let l = v & 0x0f >> 0;

                if is_left {
                    //TODO
                    s.state.A &= 0x0F;
                    s.state.A |= m;
                    s.mem_write(s.state.HL, l<<4 | h);
                    s.flags_set_to(EmulatorFlags::S, s.state.A & 0x80 != 0);

                }else{
                    s.state.A &= 0x0F;
                    s.state.A |= m;
                    s.mem_write(s.state.HL, l<<4 | h);
                    s.flags_set_to(EmulatorFlags::S, s.state.A & 0x80 != 0);
                }

                s.flags_set_to(EmulatorFlags::Z, s.state.A == 0);
                s.flags_set_to(EmulatorFlags::P, is_even_bits(s.state.A));
                s.flags_unset(EmulatorFlags::HN);
            }))
        },
        RLA | RLCA | RL | RLC | RR | RRA | RRCA | RRC | SLA | SLL | SRL | SRA => {
            let str_op = format!("{op:?}");
            let is_shift = str_op.starts_with("S");
            let is_acc = str_op.ends_with("A");
            let is_carry = str_op.contains("C");
            let is_left = str_op.contains("L");

            let (reader, writer) = if is_acc {
                    (make_reader("A"), make_writer("A"))
            } else {
                assert_eq!(1, val.len());
                (make_reader(val[0]), make_writer(val[0]))
            };

            match (reader, writer) {
                (NoneR(_s), _) | (_, NoneW(_s)) => None,
                (R8(r), W8(w)) | (R8(r), EitherW((w, _))) | (EitherR((r, _)), W8(w)) => {
                    Some(Box::new(move |s| {
                        let previous_value = r(s);
                        let (carry, new_val) = if is_left{
                            let carry = (previous_value & 0x80) != 0;
                            let mut new_val = previous_value << 1;
                            if s.flags().CF {new_val |= 0x01;}// else leave it zero
                            (carry, new_val)
                        }else{
                            let carry = (previous_value & 0x01) != 0;
                            let mut new_val = previous_value >> 1;
                            if is_shift || (is_carry && !s.flags().CF) {
                                new_val |= previous_value & 0x80
                            }else if is_carry {
                                if s.flags().CF {
                                    new_val |= 0x80;
                                }// else leave it zero
                            }
                            (carry, new_val)
                        };

                        w(s, new_val);
                        if is_carry {
                            //@CHECK : the docs say RRCA is different from RLCA. why ?
                            // let's assume symmetry
                            s.flags_set_to(EmulatorFlags::C, carry);
                        }
                        s.flags_set_to(EmulatorFlags::P, new_val.count_ones() % 2 == 0);
                        s.flags_set_to(EmulatorFlags::S, new_val & 0x80 != 0);
                        s.flags_set_to(EmulatorFlags::Z, new_val == 0);
                        s.flags_unset(EmulatorFlags::HN);
                    }))
                },
                (R16(r), W16(w)) | (EitherR((_, r)), EitherW((_, w))) | (R16(r), EitherW((_, w))) | (EitherR((_, r)), W16(w)) => {
                    Some(Box::new(move |s| {
                        let previous_value = r(s);
                        let (carry, new_val) = if is_left{
                            let carry = (previous_value & 0x8000) != 0;
                            let mut new_val = previous_value << 1;
                            if is_shift || (is_carry && !s.flags().CF) {
                                new_val |= previous_value & 0x0001
                            }else if is_carry {
                                if s.flags().CF {
                                    new_val |= 0x0001;
                                }// else leave it zero
                            }
                            (carry, new_val)
                        }else{
                            let carry = (previous_value & 0x0001) != 0;
                            let mut new_val = previous_value >> 1;
                            if is_shift || (is_carry && !s.flags().CF) {
                                new_val |= previous_value & 0x8000
                            }else if is_carry {
                                if s.flags().CF {
                                    new_val |= 0x8000;
                                }// else leave it zero
                            }
                            (carry, new_val)
                        };

                        w(s, new_val);
                        if is_carry {
                            //@CHECK : the docs say RRCA is different from RLCA. why ?
                            // let's assume symmetry
                            s.flags_set_to(EmulatorFlags::C, carry);
                        }
                        s.flags_set_to(EmulatorFlags::P, new_val.count_ones() % 2 == 0);
                        s.flags_set_to(EmulatorFlags::S, new_val & 0x8000 != 0);
                        s.flags_set_to(EmulatorFlags::Z, new_val == 0);
                        s.flags_unset(EmulatorFlags::HN);
                                        }))
                },
                (r, w) => panic!("incompatible read/write op {:?} {:?}", r.to_str(), w.to_str())
            }
            
        },
        OUTD | OUTI | INI | IND => {
            let is_in = op == INI ||op == IND;
            Some(Box::new(move |s| {
                let port =  low_byte(s.state.BC);
                if is_in {
                    let val = s.port_read(port);
                    s.mem_write(s.state.HL, val);
                }else{
                    let val = s.mem_read(s.state.HL);
                    s.port_write(port, val);
                }
                if op == OUTD {
                    s.state.HL -= 1;
                }else{
                    s.state.HL += 1;
                }
                let b = high_byte(s.state.BC);
                set_high_byte(&mut s.state.BC, b -1);
                s.flags_set_to(EmulatorFlags::Z, high_byte(s.state.BC) == 0);
                s.flags_set(EmulatorFlags::N)
            }))
        },
        IN => {
            assert_eq!(1, val.len());
            let pos : Vec<_> = val[0].split(",").collect();
            assert_eq!(2, pos.len());
            let port = pos[1];
            assert!(port == "(n)" || port == "(C)");
            let get_port = match port {
                "(n)" => |s : &mut Emulator| s.next(),
                "(C)" => |s : &mut Emulator| low_byte(s.state.BC),
                _ => unreachable!()
            };
            if let W8(w) = make_writer(pos[0]) {
                Some(Box::new(move |s| {
                    let port = get_port(s);
                    let val = s.port_read(port);
                    w(s, val);
                }))
            }else{
                None
            }
        },
        OUT => {
            assert_eq!(1, val.len());
            let pos : Vec<_> = val[0].split(",").collect();
            assert_eq!(2, pos.len());
            let port = pos[0];
            assert!(port == "(n)" || port == "(C)");
            let get_port = match port {
                "(n)" => |s : &mut Emulator| s.next(),
                "(C)" => |s : &mut Emulator| low_byte(s.state.BC),
                _ => unreachable!()
            };
            if let R8(r) = make_reader(pos[1]) {
                Some(Box::new(move |s| {
                    let port = get_port(s);
                    let val = r(s);
                    s.port_write(port, val);
                }))
            }else{
                None
            }
            
        },
        EX => {
            assert_eq!(1, val.len());
            let pos : Vec<_> = val[0].split(",").collect();
            assert_eq!(2, pos.len());
            let r0 = make_reader(pos[0]);
            let w0 = make_writer(pos[0]);
            let r1 = make_reader(pos[1]);
            let w1 = make_writer(pos[1]);
            assert!(r0.is_16() && r1.is_16() && w0.is_16() && w1.is_16());
            match(w0, w1, r0, r1) {
                (W16(w0),W16(w1),R16(r0),R16(r1))=>
                    Some(Box::new(move |s| {
                        let v0 = r0(s);
                        let v1 = r1(s);
                        w1(s, v0);
                        w0(s, v1);
                    })),
                _=>None
            }
            
        },
        EXX => Some(Box::new(|s| s.exx())),
        DAA => Some(Box::new(|s| s.daa())),
        HALT => Some(Box::new(|_s| {
            //TODO;
        })),
        DEC | INC => {
            assert_eq!(1, val.len());
            let writer = make_writer(&val[0]);
            let reader = make_reader(&val[0]);
            match (reader, writer) {
                (NoneR(_s), _) |(_, NoneW(_s)) => None,
                (R8(r), W8(w)) | (R8(r), EitherW((w, _))) | (EitherR((r, _)), W8(w)) => {
                    let mutation = match op {
                        DEC => |v| v-1,
                        INC => |v| v+1,
                        _ => unreachable!()
                    };
                    Some(Box::new(move |s| {
                                            let v = r(s);
                                            w(s, mutation(v));
                                        }))
                },
                (R16(r), W16(w)) | (EitherR((_, r)), EitherW((_, w))) | (R16(r), EitherW((_, w))) | (EitherR((_, r)), W16(w)) => {
                    let mutation = match op {
                        DEC => |v| v-1,
                        INC => |v| v+1,
                        _ => unreachable!()
                    };
                    Some(Box::new(move |s| {
                                            let v = r(s);
                                            w(s, mutation(v));
                                        }))
                },
                (r, w) => panic!("incompatible read/write op {:?} {:?}", r.to_str(), w.to_str())
            }
        },
        PUSH => {
            assert_eq!(1, val.len());
            if let R16(r) = make_reader(&val[0]) {
                Some(Box::new(move |s| {
                                        let v = r(s);
                                        s.stack_push(v);
                                    }))
            }else {
                None
            }
        },
        POP => {
            assert_eq!(1, val.len());
            if let W16(w) = make_writer(&val[0]) {
                Some(Box::new(move |s| {
                                        let v = s.stack_pop();
                                        w(s, v);
                                    }))
            }else {
                None
            }
        },
        CALL => {
            assert_eq!(1, val.len());
            assert!(val[0].ends_with("nn"));
            make_jump(val[0], true)
        },
        JR | DJNZ => {
            assert_eq!(1, val.len());
            let val = val[0];
            let condition = if op == DJNZ {
                assert_eq!("o", val);
                Box::new(|s: &mut Emulator| s.dec_b_is_zero())
            }else if val.contains(",") {
                let v : Vec<_> = val.split(",").collect();
                assert_eq!("o", v[1]);
                make_condition(v[0])
            }else{
                assert_eq!("o", val);
                make_condition("true")
            };
            Some(Box::new(move |s| {
                if condition(s) {
                    let new_val = s.state.PC as i32 + s.o() as i32;
                    s.state.PC = new_val as u16;//this may overflow.
                }
            }))
        },
        JP => {
            assert_eq!(1, val.len());
            make_jump(val[0], false)
        },
        LD => {
            assert_eq!(1, val.len());
            make_load(val[0])
        },
        CPD | CPI => {
            assert!(val.is_empty());
            let op_hl = if op == CPI {INC} else {DEC};
            let ops = [
                make_standalone(CP, &vec!["(HL)"]).unwrap(),
                make_standalone(op_hl, &vec!["HL"]).unwrap(),
                make_standalone(DEC, &vec!["BC"]).unwrap()];
            Some(Box::new(move |s| {
                let carry_was_set = s.flags().CF;
                for op in &ops {op(s)}
                s.flags_set_to(EmulatorFlags::C, carry_was_set);//The carry is preserved,
                s.flags_set(EmulatorFlags::N);//N is set
                //TODO: 
                // and all the other flags are affected as defined.
                //P/V denotes the overflowing of BC, while the Z flag is set
                //if A=(HL) before HL is decreased.
            }))//
        },
        //LOOPS
        CPDR | CPIR => {
            let op_loop = if op == CPIR {CPI} else {CPD};
            let cp = make_standalone(op_loop, &vec![]).unwrap();
            Some(Box::new(move |s| {
                while s.state.A != s.mem_read(s.state.HL) && s.state.BC != 0 {
                    cp(s);
                }
                //flags are same as CPx
            }))//
        },
        LDD | LDI => {
            assert!(val.is_empty());
            let op_hl = if op == CPI {INC} else {DEC};
            let ops = [
                make_standalone(LD, &vec!["(DE),(HL)"]).unwrap(),
                make_standalone(op_hl, &vec!["HL"]).unwrap(),
                make_standalone(op_hl, &vec!["DE"]).unwrap(),
                make_standalone(DEC, &vec!["BC"]).unwrap()];

            Some(Box::new(move |s| {
                for op in &ops {op(s)}
                s.flags_unset(EmulatorFlags::N);//N is reset
                s.flags_unset(EmulatorFlags::H);//H is reset
                //P/V is set if BC – 1 ≠ 0; otherwise, it is reset
                s.flags_set_to(EmulatorFlags::P, s.state.BC != 1);
            }))//
        },
        LDDR | LDIR => {
            let op_loop = if op == LDIR {LDI} else {LDD};
            let ld = make_standalone(op_loop, &vec![]).unwrap();
            Some(Box::new(move |s| {
                while s.state.BC != 0 {
                    ld(s);
                }
                //flags are same as LDx
            }))//
        },
        INIR | INDR | OTDR | OTIR => {
            let op_loop = match op {
                INIR => INI,
                INDR => IND,
                OTDR => OUTD,
                OTIR => OUTI,
                _ => unreachable!()
            };
            let the_op = make_standalone(op_loop, &vec![]).unwrap();
            Some(Box::new(move |s| {
                while high_byte(s.state.BC) != 0 {
                    the_op(s);
                }
                //Z is set, C is reset, N is reset, S, H, and P/V are undefined.
                s.flags_set(EmulatorFlags::Z);
                s.flags_unset(EmulatorFlags::C);
                s.flags_unset(EmulatorFlags::N);
            }))//
        },
        _ => None //panic!("not a standalone OP {:?} {:?}", x, val)
    }
}
fn make_bitwise(op : Instructions, val : &str) -> Option<Box<dyn OpMut>>{
    let pos : Vec<_> = val.split(",").collect();
    let bit_pos = pos[0].parse::<u8>().expect("bit should be [0,8)");
    let writer = make_writer(pos[1]);
    let reader = make_reader(pos[1]);
    
    use Reader::*;
    use Writer::*;
    
    match (reader, writer) {
        (R8(r), W8(w)) | ( EitherR((r, _)), EitherW((w, _)) ) =>{
            let to_do = match op {
                SET => Box::new(move |v| v | 1u8<<bit_pos) as Box<dyn Fn(u8) -> u8>,
                RES => Box::new(move |v| v & !(1u8<<bit_pos)) as Box<dyn Fn(u8) -> u8>,
                BIT => Box::new(move |v| v & !(1u8<<bit_pos)) as Box<dyn Fn(u8) -> u8>,
                _ => unreachable!()
            };
            Some(Box::new(move |s| {
                let v = r(s);
                let new_v = to_do(v);
                if op == BIT {
                    s.flags_set_to(EmulatorFlags::Z, new_v == 0);
                }else{
                    w(s, new_v);
                }
            }))
        },
        _ => None
    }
}
fn make_jump(val : &str, and_stack : bool) -> Option<Box<dyn OpMut>>{
    use Reader::*;
    let (condition, reader) = if val.contains(",") {
        let v : Vec<_> = val.split(",").collect();
        (make_condition(v[0]), make_reader(v[1]))
    }else{
        (make_condition("true"), make_reader(val))
    };
    match reader {
        R16(r) | EitherR((_, r)) =>
            Some(Box::new(move |s| {
                if condition(s) {
                    let v = r(s);
                    if and_stack {
                        s.stack_push(s.state.PC + 3);
                    }
                    s.state.PC = v;
                }
            })),
        _ => None
    }
}
fn make_load(val : &str) -> Option<Box<dyn OpMut>>{
    let pos : Vec<_> = val.split(",").collect();
    assert_eq!(2, pos.len());
    let writer = make_writer(pos[0]);
    let reader = make_reader(pos[1]);
    use Reader::*;
    use Writer::*;
    //TODO : set flags in the cases of the I or R registers.
    // C is preserved, H and N are reset, and alters Z and S. P/V is set if interrupts are enabled, reset otherwise.
    match (reader, writer) {
        (R8(r), W8(w)) | (EitherR((r, ..)), EitherW((w, _))) | (EitherR((r, ..)), W8(w)) | (R8(r), EitherW((w, ..)))=> {
            //FIXME : in the R8, W16 case, should we write (0x00VV instead)
            Some(Box::new(move |s| {
                                     let v = r(s);
                                     w(s, v);
                                 }))
        },
        (R16(r), W16(w)) | (R16(r), EitherW((_, w))) | (EitherR((_, r)), W16(w)) => {
            Some(Box::new(move |s| {
                                     let v = r(s);
                                     w(s, v);
                                 }))
        // },
        // (R8(r), W16(w)) => {
        //     Some(Box::new(move |s| {
        //                              let v = r(s) as u16;
        //                              w(s, v);//TODO: should this write one or two bytes
        //                          }))
        },
        (r, w) => {
            eprintln!("{} {}", r.to_str(), w.to_str());
            None //panic!("incompatible read/write op")
        }
    }
}
fn make_condition(s : &str) -> Box<dyn Fn(&mut Emulator) -> bool> {
    match s {
        "Z" =>Box::new(|s|s.flags().ZF),
        "NZ" =>Box::new(|s|!s.flags().ZF),
        "C" =>Box::new(|s|s.flags().CF),
        "NC" =>Box::new(|s|!s.flags().CF),
        "M" =>Box::new(|s|s.flags().SF),
        "P" =>Box::new(|s|!s.flags().SF),
        "PE" =>Box::new(|s|s.flags().PF),
        "PO" =>Box::new(|s|!s.flags().PF),
        "true" =>Box::new(|_s|true),
        inv => panic!("invalid condition {:?}", inv)
    }
}
fn make_reader(s : &str) -> Reader{
    use Reader::*;
    match s {
        //"o" => R8(Box::new(|s| s.o() )),
        "N"|"n" => R8(Box::new(|s| s.next() )),
        "NN"|"nn" => R16(Box::new(|s| s.nn() )),
        "A" => R8(Box::new(|s| s.state.A )),
        "F" => R8(Box::new(|s| s.state.F )),
        "B" => R8(Box::new(|s| high_byte(s.state.BC)  )),
        "C" => R8(Box::new(|s| low_byte(s.state.BC) )),
        "D" => R8(Box::new(|s| high_byte(s.state.DE)  )),
        "E" => R8(Box::new(|s| low_byte(s.state.DE) )),
        "H" => R8(Box::new(|s| high_byte(s.state.HL)  )),
        "L" => R8(Box::new(|s| low_byte(s.state.HL) )),
        "I" => R8(Box::new(|s| s.state.I )),
        "R" => R8(Box::new(|s| s.state.R )),
        "IXH" => R8(Box::new(|s| high_byte(s.state.IX) )),
        "IXL" => R8(Box::new(|s| low_byte(s.state.IX))),
        "IYH" => R8(Box::new(|s| high_byte(s.state.IY) )),
        "IYL" => R8(Box::new(|s| low_byte(s.state.IY))),
        "AF" => R16(Box::new(|s| to_u16(s.state.A, s.state.F))),
        "AF'" => R16(Box::new(|s| s.state.AFp)),
        "BC" => R16(Box::new(|s| s.state.BC)),
        "DE" => R16(Box::new(|s| s.state.DE)),
        "HL" => R16(Box::new(|s| s.state.HL)),
        "SP" => R16(Box::new(|s| s.state.SP)),
        "IX" => R16(Box::new(|s| s.state.IX)),
        "IY" => R16(Box::new(|s| s.state.IY)),
        "(SP)" => R16(Box::new(|s| s.mem_read16(s.state.SP))),
        "(BC)" => EitherR((
            Box::new(|s| s.mem_read(s.state.BC)),
            Box::new(|s| s.mem_read16(s.state.BC)))
        ),//TODO: should these return 16 bits ?
        "(DE)" => EitherR((
            Box::new(|s| s.mem_read(s.state.DE)),
            Box::new(|s| s.mem_read16(s.state.DE)))
            ),
        "(HL)" => EitherR((
            Box::new(|s| s.mem_read(s.state.HL)),
            Box::new(|s| s.mem_read16(s.state.HL)))
            ),
        "(IX+N)" => EitherR((
            Box::new(|s|{
                let o = s.next() as u16;
                s.mem_read(s.state.IX + o)
            }),
            Box::new(|s|{
                let o = s.next() as u16;
                s.mem_read16(s.state.IX + o)
            }))
        ),
        "(IY+N)" => EitherR((
            Box::new(|s|{
                let o = s.next() as u16;
                s.mem_read(s.state.IY + o )
            }),
            Box::new(|s|{
                let o = s.next() as u16;
                s.mem_read16(s.state.IY + o )
            }))
        ),
        "(IX)" => EitherR(
            (Box::new(|s|{
                s.mem_read(s.state.IX)
            }),
            Box::new(|s|{
                s.mem_read(s.state.IX) as u16
            }))
        ),
        "(IX+o)" => R8(
            Box::new(|s|{
                let o = s.state.IX as i16 + s.state.next_offset.unwrap() as i16;
                s.mem_read(o as u16)//FIXME possible overflow disregarded here
            })
        ),
        "(IY)" => EitherR(
            (Box::new(|s|{
                s.mem_read(s.state.IY)
            }),
            Box::new(|s|{
                s.mem_read(s.state.IY) as u16
            }))
        ),
        "(IY+o)" => R8(
            Box::new(|s|{
                let o = s.state.IY as i16 + s.state.next_offset.unwrap() as i16;
                s.mem_read(o as u16)//FIXME possible overflow disregarded here
            })
        ),
        "IXh" => R8(
            Box::new(|s|{
                let mut h = 0;
                split_u16(s.state.IX, &mut h, &mut 0);
                h
            }),
        ),
        "IXl" => R8(
            Box::new(|s|{
                let mut h = 0;
                split_u16(s.state.IX, &mut 0, &mut h);
                h
            }),
        ),
        "IYh" => R8(
            Box::new(|s|{
                let mut h = 0;
                split_u16(s.state.IY, &mut h, &mut 0);
                h
            }),
        ),
        "IYl" => R8(
            Box::new(|s|{
                let mut h = 0;
                split_u16(s.state.IY, &mut 0, &mut h);
                h
            }),
        ),
        "(NN)" => EitherR((
            Box::new(|s|{
                let addr = s.nn();
                s.mem_read(addr)
            }),
            Box::new(|s|{
                let addr = s.nn();
                s.mem_read16(addr)
            }))
        ),
        _ => NoneR(format!("unknown register {}", s).to_string())
    }
    
}
fn make_writer(s : &str) -> Writer{
    use Writer::*;
    match s {
        "A" => W8(Box::new(|s, v| s.state.A = v)),
        "F" => W8(Box::new(|s, v| s.state.F = v)),
        "B" => W8(Box::new(|s, v| set_high_byte(&mut s.state.BC, v))),
        "C" => W8(Box::new(|s, v|  set_low_byte(&mut s.state.BC, v))),
        "D" => W8(Box::new(|s, v| set_high_byte(&mut s.state.DE, v))),
        "E" => W8(Box::new(|s, v|  set_low_byte(&mut s.state.DE, v))),
        "H" => W8(Box::new(|s, v| set_high_byte(&mut s.state.HL, v))),
        "L" => W8(Box::new(|s, v|  set_low_byte(&mut s.state.HL, v))),
        "I" => W8(Box::new(|s, v| s.state.I = v )),
        "R" => W8(Box::new(|s, v| s.state.R = v )),
        "IXh" => W8(Box::new(|s, v| set_high_byte(&mut s.state.IX, v))),
        "IXl" => W8(Box::new(|s, v|  set_low_byte(&mut s.state.IX, v))),
        "IYh" => W8(Box::new(|s, v| set_high_byte(&mut s.state.IY, v))),
        "IYl" => W8(Box::new(|s, v|  set_low_byte(&mut s.state.IY, v))),
        "AF" => W16(Box::new(|s, v| split_u16(v, &mut s.state.A, &mut s.state.F))),
        "AF'" => W16(Box::new(|s, v| s.state.AFp = v)),
        "BC" => W16(Box::new(|s, v| s.state.BC = v)),
        "DE" => W16(Box::new(|s, v| s.state.DE = v)),
        "HL" => W16(Box::new(|s, v| s.state.HL = v)),
        "SP" => W16(Box::new(|s, v| s.state.SP = v)),
        "IX" => W16(Box::new(|s, v| s.state.IX = v)),
        "IY" => W16(Box::new(|s, v| s.state.IY = v)),
        "(SP)" => W16(Box::new(|s, v| s.mem_write16(s.state.SP, v))),
        "(BC)" => EitherW((
            Box::new(|s, v| s.mem_write(s.state.BC, v)),
            Box::new(|s, v| s.mem_write16(s.state.BC, v)))
        ),
        "(DE)" => EitherW((
            Box::new(|s, v| s.mem_write(s.state.DE, v)),
            Box::new(|s, v| s.mem_write16(s.state.DE, v)))
        ),
        "(HL)" => EitherW((
            Box::new(|s, v| s.mem_write(s.state.HL, v)),
            Box::new(|s, v| s.mem_write16(s.state.HL, v)))
        ),
        "(IX+N)" => EitherW((
            Box::new(|s, v|{
                let o = s.next();
                s.mem_write(s.state.IX + o as u16, v);
            }),
            Box::new(|s, v|{
                let o = s.next();
                s.mem_write16(s.state.IX + o as u16, v);
            }))
        ),
        "(IX+o)" => W8(
            Box::new(|s, v|{
                let o = s.state.IX as i16 + s.state.next_offset.unwrap() as i16;
                s.mem_write(o as u16, v)//FIXME possible overflow disregarded here
            })
        ),
        "(IY+N)" => EitherW((
            Box::new(|s, v|{
                let o = s.next();
                s.mem_write(s.state.IY + o as u16, v);
            }),
            Box::new(|s, v|{
                let o = s.next();
                s.mem_write16(s.state.IY + o as u16, v);
            }))
        ),
        "(IY+o)" => W8(
            Box::new(|s, v|{
                let o = s.state.IY as i16 + s.state.next_offset.unwrap() as i16;
                s.mem_write(o as u16, v)//FIXME possible overflow disregarded here
            })
        ),
        "(NN)" => EitherW((
            Box::new(|s, v|{
                let addr = s.nn();
                s.mem_write(addr, v);
            }),
            Box::new(|s, v|{
                let addr = s.nn();
                s.mem_write16(addr, v);
            }))
        ),
        _ => NoneW(format!("unknown register {}", s).to_string())
    }

}

fn make_op(def : & OpcodeDef, depth : usize) -> Option<Operation>{
    let codes = def.codes[depth..].to_vec();
    let cycles = def.cycles;
    let contents : Vec<_> = def.op.split_whitespace().collect();
    let op = Instructions::from_str(contents[0]).expect("unexpected instruction");
    let contents = &contents[1..];
    assert!(contents.len() <= 2);
    let ok = match codes.len() {
        1 => true,
        2 => {
            assert!(codes[1] == "n" || codes[1] == "o", "expected n | o, got {:?}", codes);
            true
        },
        3 =>{
            // assert_eq!("nn", codes[1], "expected nn, got {:?}", codes);
            // assert_eq!("nn", codes[2], "expected nn, got {:?}", codes);
            true
        },
        _ => {
            console::log_1(&format!("unexpected def : {:?}", def).into());
            false
        }
    };
    if ok {
        let to_do = make_standalone(op, contents);
        if let Some(to_do) = to_do {
            Some(Box::new(move |s| {
                to_do(s);
                cycles
            }))
        }else{
            None
        }
    }else{
        None
    }
}

/**
 * makes a series of consecutive opcodes starting at a given byte and 
 * puts them in the given opcode table
 * */
fn make_composite(opcodes : &mut Opcodes, key : &str, def : &OpcodeDef, recursion_depth : usize) -> (usize, usize) {
    if key.contains("+8*b") {
        return expand_bits(opcodes, key, def, recursion_depth);
    }
    let found_parts = match key {
        key if key.ends_with("+r") => Some((
                u8::from_str_radix(key.trim_end_matches("+r"), 16),//base
                vec![("A", 7u8),("B", 0u8),("C", 1u8),("D", 2u8),
                    ("E", 3u8),("H", 4u8),("L", 5u8)], // normal registers
                "r"
            )),
        key if key.ends_with("+p") && def.op.contains("IXp") => Some((
                u8::from_str_radix(key.trim_end_matches("+p"), 16),//base
                vec![("IXh", 4u8),("IXl", 5u8)], // X only two registers
                "IXp"
            )),
        key if key.ends_with("+8*p") => Some((
                u8::from_str_radix(key.trim_end_matches("+8*p"), 16),//base
                vec![("A", 8*7u8),("B", 8*0u8),("C", 8*1u8),("D", 8*2u8),
                    ("E", 8*3u8),("IXh", 8*4u8),("IXl", 8*5u8)], // all X registers
                "IXp"
            )),
        key if key.ends_with("+p") => Some((
                u8::from_str_radix(key.trim_end_matches("+p"), 16),//base
                vec![("A", 7u8),("B", 0u8),("C", 1u8),("D", 2u8),
                    ("E", 3u8),("IXh", 4u8),("IXl", 5u8)], // all X registers
                "p"
            )),
        key if key.ends_with("+q") && def.op.contains("IYq") => Some((
                u8::from_str_radix(key.trim_end_matches("+q"), 16),//base
                vec![("IYh", 4u8),("IYl", 5u8)], // Y only two registers
                "IYq"
            )),
        key if key.ends_with("+q") => Some((
                u8::from_str_radix(key.trim_end_matches("+q"), 16),//base
                vec![("A", 7u8),("B", 0u8),("C", 1u8),("D", 2u8),
                    ("E", 3u8),("IYh", 4u8),("IYl", 5u8)], // all Y registers
                "q"
            )),
        key if key.ends_with("+8*q") => Some((
                u8::from_str_radix(key.trim_end_matches("+8*q"), 16),//base
                vec![("A", 8*7u8),("B", 8*0u8),("C", 8*1u8),("D", 8*2u8),
                    ("E", 8*3u8),("IYh", 8*4u8),("IYl", 8*5u8)], // all Y registers
                "IYq"
            )),
        _ => None
    };

    if let Some((Ok(base), registers, replacement)) = found_parts{
        let mut ok = 0;
        for (name, offset) in &registers {
            let op = def.op.replace(replacement, name);
            let def_copy = OpcodeDef{
                op: &op,
                codes: def.codes.to_vec(),
                cycles: def.cycles
            };
            if let Some(op) = make_op(&def_copy, recursion_depth){
                opcodes[base+offset] = op;
                ok +=1;
            }else{
                // console::log_1(&format!(".. >replaced composite opcode {} ({}): {} -> {}", base, replacement, def.op, op).into());
                console::log_1(&format!(".. >missed composite opcode : {:?} ({} -> {})", def_copy, def.op, op).into());
            }
        }
        if ok != registers.len() {
            console::log_1(&format!(".. >>multi opcode : {:?} ({})", def.codes, def.op).into());
        }
        (registers.len(), ok)
    }else{
        (7, 0)
    }
}

/**
 * creates a series of BIT-wise opcodes and puts them
 * in the given Opcodes table 
 */
fn expand_bits(opcodes : &mut Opcodes, key : &str, def : &OpcodeDef, recursion_depth : usize) -> (usize, usize){
    if let [base, end] = key.split("+8*b").collect::<Vec<_>>()[..] {
        let mut ok = 0;
        let mut total = 0;
        let is_composite = end != "";
        let base_byte = u8::from_str_radix(base, 16).expect(&format!("base of bitwise op MUST be a byte, but is {base}"));
        for bit in 0..8u8 {//8 ops for the 8 possible bits
            let byte = base_byte + bit;
            let op = def.op.replace("b", &bit.to_string());
            let def_copy = OpcodeDef{
                op: &op,
                codes: def.codes.to_vec(),
                cycles: def.cycles
            };
            if is_composite {
                let (todo, done) = make_composite(opcodes, &format!("{base}{end}"), &def_copy, recursion_depth);
                total += todo;
                ok += done;
            }else{
                total +=1;
                if let Some(op) = make_op(&def_copy, recursion_depth){
                    opcodes[byte] = op;
                    ok +=1;
                }else{
                    console::log_1(&format!(".. >missed composite BITWISE opcode : {:?} ({} -> {})", def_copy, def.op, op).into());
                }
            }
        }
        (total, ok)
    }else{
        console::log_1(&format!(".. >BITWISE opcode : {:?} not splittable", key).into());
        (8, 0)
    }

}


#[derive(Debug, PartialEq, EnumString, Clone, Copy)]
enum Instructions{
    ADC,ADD,AND,BIT,CALL,CCF,CP,CPD,CPDR,CPI,CPIR,CPL,DAA,DEC,DI,DJNZ,EI,EX,EXX,
    HALT,IM,IN,INC,IND,INDR,INI,INIR,JP,JR,LD,LDD,LDDR,LDI,LDIR,NEG,NOP,OR,OTDR,
    OTIR,OUT,OUTD,OUTI,POP,PUSH,RES,RET,RETI,RETN,RL,RLA,RLC,RLCA,RLD,RR,RRA,RRC,
    RRCA,RRD,RST,SBC,SCF,SET,SLA,SLL,SL1,SRA,SRL,SUB,XOR}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_IX_registers() {
        let def = OpcodeDef{
                op: "LD IXl,IXh",
                codes: vec!["DD", "68+p"],
                cycles: 8
            };

        assert!(make_op(&def, 1).is_some());
    }

    #[test]
    fn test_OP_names() {
        let def = Instructions::RLC;
        let def_str = format!("{def:?}");

        assert_eq!("RLC", def_str);
    }

    #[test]
    fn test_OP_parse() {
        let ld = make_standalone(LD, &vec!["(DE),(HL)"]);
        assert!(ld.is_some());
        let inFC = make_standalone(IN, &vec!["F,(C)"]);
        assert!(inFC.is_some());
        let inFC = make_standalone(JP, &vec!["(IX)"]);
        assert!(inFC.is_some());
    }
    ///this test just verifies my assumption for expand_bits
    #[test]
    fn test_string_split() {
        let ld = "test**";
        let parts = ld.split("**").collect::<Vec<_>>();
        assert_eq!(2, parts.len());
        assert_eq!(parts[1], "");
    }
}

