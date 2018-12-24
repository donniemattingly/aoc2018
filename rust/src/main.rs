// Stole this setup from https://www.reddit.com/r/adventofcode/comments/a7j9zc/2018_day_19_solutions/ec3i62o/
// to avoid rewriting it

use std::fs::{File};
use std::io::{BufReader, BufRead};
use std::collections::HashSet;

type Regs = [isize; 6];
struct OpCode {
    code: isize,
    a: isize,
    b: isize,
    c: isize,
}

fn eval(op: &OpCode, regs: &mut Regs) {
    let code = op.code;
    regs[op.c as usize] = match code {
        0x0 => regs[op.a as usize] + regs[op.b as usize],
        0x1 => regs[op.a as usize] + op.b,
        0x2 => regs[op.a as usize] * regs[op.b as usize],
        0x3 => regs[op.a as usize] * op.b,
        0x4 => regs[op.a as usize] & regs[op.b as usize],
        0x5 => regs[op.a as usize] & op.b,
        0x6 => regs[op.a as usize] | regs[op.b as usize],
        0x7 => regs[op.a as usize] | op.b,
        0x8 => regs[op.a as usize],
        0x9 => op.a,
        0xA => (op.a > regs[op.b as usize]) as isize,
        0xB => (regs[op.a as usize] > op.b) as isize,
        0xC => (regs[op.a as usize] > regs[op.b as usize]) as isize,
        0xD => (op.a == regs[op.b as usize]) as isize,
        0xE => (regs[op.a as usize] == op.b) as isize,
        0xF => (regs[op.a as usize] == regs[op.b as usize]) as isize,
        _ => unreachable!()
    }
}

fn parse_instr(s: &str) -> isize {
    match s {
        "addr" => 0x0,
        "addi" => 0x1,
        "mulr" => 0x2,
        "muli" => 0x3,
        "banr" => 0x4,
        "bani" => 0x5,
        "borr" => 0x6,
        "bori" => 0x7,
        "setr" => 0x8,
        "seti" => 0x9,
        "gtir" => 0xA,
        "gtri" => 0xB,
        "gtrr" => 0xC,
        "eqir" => 0xD,
        "eqri" => 0xE,
        "eqrr" => 0xF,
        _ => unreachable!(),
    }
}


fn main() {
    let file = File::open("../inputs/input-21.1.txt").unwrap();
    let reader = BufReader::new(file);
    let mut instrs = vec![];                                              
    let mut lines = reader.lines();                                       
    let ip = lines.next().expect("expected at least one line").unwrap();           
    let ipr: usize = ip.split_whitespace().skip(1).next().unwrap().parse().unwrap();

    for line in lines.filter_map(Result::ok) {                            
        let mut words = line.split_whitespace();                          
        let code = parse_instr(words.next().unwrap());                    
        let a = words.next().unwrap().parse().unwrap();                           
        let b = words.next().unwrap().parse().unwrap();                           
        let c = words.next().unwrap().parse().unwrap();                           
        instrs.push(OpCode {code, a, b, c});                              
    }                                                                     

    let mut regs = [0; 6];                                                
    let mut ip = 0;         

    let mut endVals = HashSet::new();
    let mut endRepeats = false;

    // guessed 1797946
    // regs[0] = 16311888;
        while ip < instrs.len(){                                             
        eval(&instrs[ip], &mut regs);                                     
        regs[ipr] += 1;                                                   
        ip = regs[ipr] as usize; 
        

        if(ip == 29){
            if(endVals.get(&regs[3]).is_some()){
                println!("val: {}", regs[3]);
                println!("Found it!");
                break;
            } else {
                endVals.insert(regs[3]);
            } 
        }
    }
    println!("ip: {} regs: {}, {}, {}, {}, {}, {}", ip, regs[0], regs[1], regs[2], regs[3], regs[4], regs[5]);                                                                     

}
