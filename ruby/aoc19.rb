

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


