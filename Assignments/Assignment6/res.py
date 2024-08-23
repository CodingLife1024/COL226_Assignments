import sys
import ast
import operator
from collections import namedtuple

State = namedtuple('State', ('s', 'e', 'c', 'd'))

APPLY = lambda S, N, F: F(*[x.pop() for x in [S.s] * N])
PEEK = lambda S, N, F: F(*S.s[-1:0-N])

def extract_e_c(_, e, c, __):
    return [e[:], c.pop()]

COMMANDS = {
	'ADD': (APPLY, 2, operator.add),
	'MUL': (APPLY, 2, operator.mul),
	'SUB': (APPLY, 2, operator.sub),
	'DIV': (APPLY, 2, operator.div),
	'XOR': (APPLY, 2, operator.xor),
	'EQ': (PEEK, 2, operator.eq),
	'LT': (PEEK, 2, operator.lt),
	'GT': (PEEK, 2, operator.gt),
	'LEQ': (PEEK, 2, operator.le),
	'GEQ': (PEEK, 2, operator.ge),
	'CONS': (APPLY, 2, lambda car, cdr: [cdr.append(car), cdr][1]),
	'CAR': (APPLY, 1, lambda x: x.pop()),
	'CDR': (APPLY, 1, lambda xs: [xs.pop(), xs][1]),
	'NIL': (lambda _: list(),),
	'ATOM': (PEEK, 1, lambda x: x == []),
	'LDC': (lambda S: S.c.pop(),),  # pushes constant argument onto stack
	'LDF': (extract_e_c,)
}

def secd_eval(code, stack=[]):
	state, running = State(stack, [], code, []), True
	while running:
		state, running = secd_step(state)
	return state

def secd_step(state):
    s, e, c, d = state
    cmd = c.pop().upper()
    stop = False

    if cmd in COMMANDS:
        X = COMMANDS[cmd]
        s.append(X[0](*[state] + X[1:]))
    elif cmd == 'STOP':
        stop = True
    elif cmd == 'AP':
        d.extend([c, e, s])
        ce = s.pop()
        c, e = ce.pop(), ce.pop()
        e.append(s.pop())
        s = []
    elif cmd == 'LD':
        ((x, y),) = c.pop()
        s.append(e[-x][-y])
    elif cmd == 'DUM':
        e.append([])
    elif cmd == 'RAP':
        closure = s.pop()
        fn, env = closure.pop(), closure.pop()
        env.pop()
        v = s.pop()
        e.pop()
        d.extend([c, e, s])
        e = env
        e.append(v)
        c = fn
        s = []
    elif cmd == 'JOIN':
        c = [d.pop()]
    elif cmd == 'RTN':
        retval = s.pop()
        s = d.pop()
        s.append(retval)
        e, c = d.pop(), d.pop()
    elif cmd == 'SEL':
        x = s.pop()
        ct, cf = c.pop(), c.pop()
        d.append(c)
        c = [ct if x else cf]
    else:
        raise ValueError("Invalid command")

    return State(s, e, c, d), not stop


if __name__ == "__main__":
    code = ast.literal_eval(sys.stdin.read())
    result = secd_eval(code)
