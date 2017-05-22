#!/usr/bin/env python3

n2b = lambda n, d: [bool(n >> i & 1) for i in range(d)][::-1]
b2n = lambda b: sum(int(d)<<i for (i, d) in enumerate(b[::-1]))
bs = lambda d: [n2b(n, d) for n in range(1<<d)]
ns = lambda b: [b2n(n) for n in b]

s = [(lambda x:x) for _ in range(10)]
s[1] = lambda n: [not n[0]]
s[2] = lambda n: [n[0] ^ n[1], not n[1]]

test = lambda d: ns(s[d](n) for n in bs(d))
