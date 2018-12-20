def input
  `cat ../inputs/input-19.1.txt`
    .split("\n")
    .reject(&:empty?)
    .compact
end

def parse_instr(line)
  parts = line.split(" ").compact
  {op: parts[0].to_sym, a: parts[1].to_i, b: parts[2].to_i, c: parts[3].to_i}
end

registers = [1,0,0,0,0,0]

instructions = {
  addr: ->(a, b, c, r) { r[c] = r[a] + r[b] },
  addi: ->(a, b, c, r) { r[c] = r[a] + b },
  mulr: ->(a, b, c, r) { r[c] = r[a] * r[b] },
  muli: ->(a, b, c, r) { r[c] = r[a] * b },
  banr: ->(a, b, c, r) { r[c] = r[a] & r[b] },
  bani: ->(a, b, c, r) { r[c] = r[a] & b },
  borr: ->(a, b, c, r) { r[c] = r[a] | r[b] },
  bori: ->(a, b, c, r) { r[c] = r[a] | b },
  setr: ->(a, _, c, r) { r[c] = r[a] },
  seti: ->(a, _, c, r) { r[c] = a },
  gtir: ->(a, b, c, r) { r[c] = a > r[b] ? 1 : 0 },
  gtri: ->(a, b, c, r) { r[c] = r[a] > b ? 1 : 0 },
  gtrr: ->(a, b, c, r) { r[c] = r[a] > r[b] ? 1 : 0 },
  eqir: ->(a, b, c, r) { r[c] = a == r[b] ? 1 : 0 },
  eqri: ->(a, b, c, r) { r[c] = r[a] == b ? 1 : 0 },
  eqrr: ->(a, b, c, r) { r[c] = r[a] == r[b] ? 1 : 0 },
}.freeze


ip = input()[0][-1].to_i
instrs = input()[1..-1].map {|l| parse_instr(l)}
str_instrs = input()[1..-1]

cur_instr = registers[ip]
while cur_instr < instrs.length
  inst = instrs[cur_instr]
  # puts inst
  # puts registers
  fn = instructions[inst[:op]]
  fn.(inst[:a], inst[:b], inst[:c], registers)
  registers[ip] = registers[ip] + 1
  cur_instr = registers[ip]
  puts "#{cur_instr} #{inst}"
  # puts "#{cur_instr} #{registers}"
  puts "#{registers}"
  puts "#{str_instrs[cur_instr]}"
end

puts "#{registers}"
