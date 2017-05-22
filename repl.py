#!/usr/bin/env python3

"""
set x := term
    = x term

load x
    : x
controls
    next: ,
    prev: .
    stop: /

term syntax
    pi: {x y z : z y x}
    bullet: . () _

"""

defs = {}

def termeq(t1, t2):
    if type(t1) == type(t2):
        if type(t1) == list:
            return all(termeq(p,q) for p,q in zip(t1,t2))
        elif type(t1) == TermNil:
            return True
        elif type(t1) == TermPi:
            return t1._p == t2._p and t1._s == t2._s
        elif type(t1) == TermApp:
            return termeq(t1._terms, t2._terms)
        else:
            return t1 == t2
    return False

class Term:
    _name = False
    def name(self, n):
        defs[n] = self
        self._name = n

    def succ(self): pass
    def pred(self): pass
    def fw(*a): pass
    def bw(*a): pass

    def _repr(self):
        return '<Term>'

    def __repr__(self):
        if self._name:
            return self._name
        return self._repr()

class TermPi(Term):
    def __init__(self, p, s):
        self._p = p
        self._s = s

    def _repr(self):
        delim = 'π'
        nil = 'ø'

        def build(ls):
            res = []
            for l in ls:
                if type(l) == str:
                    if l == '.':
                        res.append(nil)
                    else:
                        res.append(l)
                elif type(l) == list:
                    res.append('(%s)' % build(l))
                else:
                    res.append('<? %r ?>'%l)
            return ' '.join(res)
        return '%s %s : %s %s' % (delim, build(self._p), build(self._s), delim)


    def fw(self, terms):
        return self.app(self._p, terms, self._s)

    def bw(self, terms):
        return self.app(self._s, terms, self._p)

    def app(self, ls, ts, ms):
        binds = {'_': self}
        fail = False
        #print(ls, ts, ms)

        def bind(ls, ts):
            #print('bind:', ls, ts)
            if len(ls) != len(ts):
                fail = True
                return

            for l,t in zip(ls, ts):
                if l == '.':
                    if t != '.' and type(t) != TermNil:
                        fail = True
                        return
                else:
                    if type(l) == str:
                        if l in binds and not termeq(t, binds[l]):
                            #print("couldn't resolve duplicate")
                            fail = True
                            return
                        binds[l] = t
                    else:
                        if type(l) != list or type(t) != TermApp:
                            fail = True
                            return
                        bind(l, t._terms)

        bind(ls, ts)
        if fail:
            #print('fail')
            return False

        def insert(ms):
            ns = []
            for m in ms:
                if type(m) == list:
                    ns.append(insert(m))
                elif m == '.':
                    ns.append(TermNil())
                elif type(m) == str and m in binds:
                    ns.append(binds[m])
                else:
                    fail = True
                    return

            return TermApp(ns)

        res = insert(ms)
        if fail:
            #print('fail')
            return False
        #print(binds)
        #print(res)
        return res._terms


class TermNil(Term):
    def _repr(self):
        return 'ø'

class TermApp(Term):
    def __init__(self, terms):
        self._terms = terms

    def succ(self):
        s = False
        for t in self._terms:
            try:
                s |= t.succ()
            except AttributeError:
                pass
        if s: return s


        pi, *ts = self._terms
        if type(pi) != TermPi:
            return
        perm = pi.fw(ts)
        if perm:
            perm.append(pi)
            self._terms = perm
        return bool(perm)

    def pred(self):
        s = False
        for t in self._terms:
            try:
                s |= t.pred()
            except AttributeError:
                pass
        if s: return s

        *ts, pi = self._terms
        if type(pi) != TermPi:
            return
        perm = pi.bw(ts)
        if perm:
            perm.insert(0, pi)
            self._terms = perm
        return bool(perm)

    def _repr(self):
        return '(%s)' % ' '.join(repr(t) for t in self._terms)
    __repr__ = _repr




    

"""
class Term:
    pass

class TermPi(Term):
    def __init__(self, 
    pass

class TermApp(Term):
    def __init__(self, terms):
        self._terms = terms
    pass
"""

def parse_pi_(s, n=0):
    terms = []
    while True:
        s = s.strip()
        if not s:
            break
        if s[0] == ')':
            s = s[1:]
            break

        if s[0] == '(':
            t, s = parse_pi_(s[1:], n+1)
            terms.append(t)
        else:
            t, _, s = s.partition(' ')
            terms.append(t)
    return terms, s

def parse_pi(s1, s2):
    t1, _ = parse_pi_(s1)
    t2, _ = parse_pi_(s2)
    return TermPi(t1, t2)
    return (t1, t2)

def parse(s, n=0):
    terms = []
    while True:
        s = s.strip()
        if not s:
            break
        if s[0] == ')':
            s = s[1:]
            break

        if s[0] == '.':
            terms.append(TermNil())
            s = s[1:]

        elif s[0] == '{':
            t, _, s = s[1:].partition('}')
            t1, _, t2 = t.partition(':')
            terms.append(parse_pi(t1, t2))
        elif s[0] == '(':
            t, s = parse(s[1:], n+1)
            terms.append(t)
        else:
            t, _, s = s.partition(' ')
            if t in defs:
                t = defs[t]
            terms.append(t)

    if n==0 and len(terms) == 1:
        terms = terms[0]
    elif terms:
        terms = TermApp(terms)
    else:
        terms = TermNil()

    return terms, s






if __name__ == '__main__':
    import readline
    prompt = '> '
    terms = [parse('()')[0]]
    index = 0

    while True:
        curr = terms[index]
        try:
            inp = str(input(prompt)).strip()

            if not inp:
                continue
            if inp[0] == '=':
                label, _, s = inp[1:].strip().partition(' ')
                t, _ = parse(s)
                t.name(label)
                #t = defs[label] = parse(s)
                print(label, '=>', t._repr())
            elif inp[0] == '@':
                print('[%d defs:]' % len(defs))
                for l,t in defs.items():
                    print (' ', l, '=>', t._repr())
            elif inp[0] == ':':
                s = inp[1:].strip()
                t, _ = parse(s)
                terms.append(t)
                index = len(terms) - 1
                curr = terms[index]
                print(curr)
            elif inp[0] == ';':
                n = int(inp[1:]) - 1
                index = sorted((0,n,len(terms)-1))[1]
                curr = terms[index]
                print(curr)
            elif inp[0] == '*':
                print('[%d terms:]' % len(terms))
                for t in terms:
                    print (' ', t)
            elif inp[0] == '0':
                print(curr)
            elif inp[0] == '-':
                n = int(inp[1:]) if inp[1:] else 1
                print(curr)
                for _ in range(n):
                    curr.pred()
                    print(curr)
            elif inp[0] == '+':
                n = int(inp[1:]) if inp[1:] else 1
                print(curr)
                for _ in range(n):
                    curr.succ()
                    print(curr)

        except EOFError:
            print('...bye')
            break
        except KeyboardInterrupt:
            print()
        """except Exception as e:
            print()
            print('unknown error:', e)"""
