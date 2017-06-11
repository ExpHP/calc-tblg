#!/usr/bin/env python3

# Transform haskell output into less sucky json and compute positions.
# (implicit units of C-C bond length)

from moire.exact import MoirePattern, find_nicer_cell
from sympy import S, Matrix, sqrt, oo, rot_axis3, atan2, simplify

import itertools
import sys
import json

class Layout:
    VOLUME_LETTER = 'volume'
    ABC_SCALE = 'a-b-c-n-d-k'
    all = [VOLUME_LETTER, ABC_SCALE]

def main():
    global PARANOID

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--stream',
        action='store_true',
        help="outputting json dicts one by one instead lf a list."
        " The generated file is technically invalid json, but jq will gobble it up."
    )
    parser.add_argument('--paranoid', '-P', default=0, action='count')
    parser.add_argument('--carefree', '-C', default=0, action='count')
    parser.add_argument(
        '--min-volume', '-n', default=0, type=int, help='inclusive')
    parser.add_argument(
        '--max-volume', '-N', default=None, type=int, help='exclusive')
    parser.add_argument(
        '--key-layout', '-k', default=Layout.VOLUME_LETTER, choices = Layout.all)
    args = parser.parse_args()

    PARANOID = args.paranoid - args.carefree

    if sys.stdin.isatty:
        print("Reading from standard input...", file=sys.stderr)

    run = lambda x: main_(x, args.key_layout, args.min_volume, args.max_volume)
    it = (run(soln) for soln in json.load(sys.stdin))
    dump = lambda x: json.dump(x, sys.stdout)

    def passthru(testFunc, x):
        testFunc(x)
        return x
    if PARANOID >= 1:
        it = (passthru(do_special_case_sanity_checks, x) for x in it)

    if args.stream:
        for x in it:
            if x: dump(x)
    else: dump([x for x in it if x])


def main_(soln, key_layout, min_volume, max_volume):
    # my apologies for the dreadfully short variable names with a dreadfully long scope...
    beta, (a, b, c), rparts = soln
    rn = int(rparts['numerator'])
    rd = int(rparts['denominator'])
    rk = int(rparts['rootNum'])
    r = S(rn) / rd * sqrt(rk)

    # family matrices and sites ought to be added to input
    #  before we start trying anything but hex
    assert beta == 3

    # HACK: show the ABC_SCALE key for progress feedback. (VOLUME_LETTER key cannot be computed yet)
    print("handling {}-{}-{}-{}-{}-{}".format(a,b,c,rn,rd,rk), file=sys.stderr)

    sites = [[0, 0], [S(2) / 3, S(1) / 3]]
    A = Matrix([[1, 0], [-S(1) / 2, sqrt(3) / 2]])

    M = S(r) / c * Matrix([[a, -b * sqrt(beta)], [b * sqrt(beta), a]])
    B = mdot(A, M.T)

    mp = MoirePattern.from_cells(A, B)

    SC = find_nicer_cell(mp.commensurate_cell())
    # rotate/reflect basis for lammps
    (SC, trans) = lammps_friendly_cell(SC)
    SC = no_60s_allowed(SC)
    A = mdot(A, trans)
    B = mdot(B, trans)

    C = mdot(SC, A.inv())
    D = mdot(SC, B.inv())

    volumes = [abs(C.det()), abs(D.det())]
    if max_volume is not None and S(max_volume) <= S(volumes[0]):
        return None
    if S(volumes[0]) < S(min_volume):
        return None

    def compute_key():
        if key_layout == Layout.ABC_SCALE:
            key_parts = [a, b, c, rn, rd, rk]
            key_string = '-'.join(map(str, map(int, key_parts)))

        elif key_layout == Layout.VOLUME_LETTER:
            assert rparts['numerator'] == rparts['denominator'] == rparts['rootNum'] == 1

            # encode just volume, and disambiguate using letters
            # HACK: hardcoded disambiguation scheme for hexagonal.
            #       we classify into 30 degree ranges a=[0,30), b=[30,60), c=[60,90)
            import math
            if (a,b,c) == (1,1,2): # 60 degrees exact
                letter = 'b'
            else: # we can trust floating point precision for the rest
                letter = chr(ord('a') + int(math.acos(a/c) // (math.pi / 6)))

            v = int(volumes[0]) # de-sympify due to poor support for format specs
            key_parts = [v, letter]
            key_string = '{:03d}-{}'.format(*key_parts)

        else:
            raise RuntimeError("incomplete switch for Layout")

        return dict(layout=key_layout, parts=key_parts, string=key_string)

    key = compute_key()

    # FIXME:  HACK:
    # check that the cell is "standard" for hexagonal;
    # both vectors should be of equal length
    assert sqnorm(SC[:2]) == sqnorm(SC[2:])

    #indices = [[i+di,j+dj] for i in range(100) for j in range(100) for (di,dj) in sites]

    # A basis or B basis (xxLatt are integer coords)
    csLatt = list(supercellPoints(C))
    dsLatt = list(supercellPoints(D))
    csSites = [(i + di, k + dk) for (i, k) in csLatt for (di, dk) in sites]
    dsSites = [(i + di, k + dk) for (i, k) in dsLatt for (di, dk) in sites]
    # S basis
    csLatt = list(map(fracModMatrix(C), csLatt))
    csSites = list(map(fracModMatrix(C), csSites))
    dsLatt = list(map(fracModMatrix(D), dsLatt))
    dsSites = list(map(fracModMatrix(D), dsSites))

    if PARANOID >= 0:
        assert len(csSites) == len(set(csSites))
        assert len(dsSites) == len(set(dsSites))

        assert len(csLatt) == C.det()
        assert len(dsLatt) == D.det()

        assert len(csSites) == C.det() * len(sites)
        assert len(dsSites) == D.det() * len(sites)

    def validate_hexagonal_shape(fracs):
        fracs = [(i + di, k + dk)
                 for (i, k) in fracs
                 for (di, dk) in itertools.product([-1, 0, 1], repeat=2)]
        carts = list(map(mulMatrix(SC), fracs))
        carts.sort(key=sqnorm)
        carts = carts[::-1]
        assert sqnorm(carts.pop()) == 0
        # 6 nearest neighbors
        u = [carts.pop() for _ in range(6)]
        z = carts.pop()
        assert all(sqnorm(x) == sqnorm(u[0]) for x in u), list(map(sqnorm, u))
        assert sqnorm(u[0]) < sqnorm(z)

    def validate_honeycomb_shape(fracs):
        fracs = [(i + di, k + dk)
                 for (i, k) in fracs
                 for (di, dk) in itertools.product([-1, 0], repeat=2)]
        carts = list(map(mulMatrix(SC), fracs))
        carts.sort(key=sqnorm)
        carts = carts[::-1]
        assert sqnorm(carts.pop()) == 0
        u = carts.pop()
        v = carts.pop()
        w = carts.pop()
        z = carts.pop()
        # 3 nearest neighbors
        assert sqnorm(u) == sqnorm(v) == sqnorm(w), '{} {} {}'.format(
            *map(sqnorm, (u, v, w)))
        assert sqnorm(u) < sqnorm(z)
        assert abs(dot(u, v)) == abs(dot(v, w)) == abs(dot(w, u))
        assert abs(dot(u, v)) / (
            norm(u) * norm(v)) == S(1) / 2, abs(dot(u, v)) / norm(u)

    if PARANOID >= 1:
        validate_hexagonal_shape(csLatt)
        validate_hexagonal_shape(dsLatt)

        validate_honeycomb_shape(csSites)
        validate_honeycomb_shape(dsSites)

        validate_standard_hex_cell(SC)
        validate_standard_hex_cell(A)
        validate_standard_hex_cell(B)

    uniter2 = lambda f, it: [[f(x) for x in xs] for xs in it]
    unmat = lambda f, M: uniter2(f, M.tolist())

    return {
        'key': key,
        'lattice': unmat(float, SC),
        'A': uniter2(float, csSites),
        'B': uniter2(float, dsSites),
        'meta': {
            'abc': [a, b, c],
            'β': beta,
            'families': [
                [[1, 2], [1, 2]],
                [[1, 2], [1, 2]],
            ],
            'A': {
                'cart': {'approx': unmat(float, A)},
                'frac': {'approx': unmat(float, C.inv())},
            },
            'B': {
                'cart': {'approx': unmat(float, B)},
                'frac': {'approx': unmat(float, D.inv())},
            },
            'C': unmat(int, C),
            'D': unmat(int, D),
            'volume': {
                # in units of...
                'A': abs(int(C.det())),
                'B': abs(int(D.det())),
            },
            'S': {'approx': unmat(float, SC)},
            'r': {
                'square': [rn * rn * rk, rd * rd],
                'exact': [rn, rd, rk],
                'approx': float(r),
            },
        },
    }


def lammps_friendly_cell(m):
    m = Matrix(m)
    # This cannot be achieved solely through a unitary transform;
    # An additional cartesian transformation must be performed, which is
    #  returned in 'trans' (and must be applied to A and B to preserve
    #  the supercell matrices C and D)
    # rotate a to +x
    rot = rot_axis3(-atan2(m[1], m[0]))

    # sympy rotation matrix is apparently [[+cos,+sin],[-sin,cos]].
    # I can't say I've *ever* seen this convention before... :/
    rot = rot.T

    rot.col_del(2)
    rot.row_del(2)

    trans = simplify(rot.T)
    m = mdot(m, trans).as_mutable()

    # maybe negate second basis vector
    # (this is unitary and can be left out of 'trans')
    if m[3] < 0:
        m[2] = -m[2]
        m[3] = -m[3]
    # check
    assert m[1] == 0, str(m)
    assert m[0] > 0, str(m)
    assert m[3] > 0, str(m)
    return (m, trans)


# Input:  Integer supercell matrix
# Output: Integer coords of unitcell lattice points in supercell
def supercellPoints(m):
    getfrac = fracModMatrix(m)
    seen = set()

    # how many points to a row? (all rows will share the same number, because the lengths
    #  of parallel lines between two parallel lines are equal)
    for j in itertools.count(1):
        if getfrac((0, j)) == (0, 0):
            nj = j
            break

    for i in itertools.count(0):
        # find first new column in this row
        for j in range(nj):
            if getfrac((i, j)) not in seen:
                break
        # went a whole period; there's nothing new left
        else:
            break

        ijs = [(i, j) for j in range(j, j + nj)]
        fracs = set(getfrac(ij) for ij in ijs)
        assert len(ijs) == len(fracs)
        seen.update(fracs)
        yield from ijs


# =================================
# NOTE: These all take a single vector as their second (curried) argument.

# IMPORTANT: These are for row-based vector formalisms
def mulMatrix(m):
    return lambda ij: tuple((Matrix([list(ij)]) * m))


def truedivMatrix(m):
    mInv = m.inv()
    return lambda ij: tuple((Matrix([list(ij)]) * mInv))


def fracModMatrix(m):
    mInv = m.inv()
    return lambda ij: tuple((Matrix([list(ij)]) * mInv).applyfunc(lambda x: x % 1))


def modMatrix(m):
    mInv = m.inv()
    return lambda ij: tuple((Matrix([list(ij)]) * mInv).applyfunc(lambda x: x % 1) * m)
# =================================


def diff(xy1, xy2):
    assert len(xy1) == len(xy2)
    return tuple(a - b for (a, b) in zip(xy1, xy2))


def norm(x):
    return sqrt(sqnorm(x))


def sqnorm(x):
    return dot(x, x)


def dot(x, y):
    (x, y) = (list(x), list(y))
    assert len(x) == len(y)
    return sum(a * b for (a, b) in zip(x, y))


def cross2(xy1, xy2):
    assert len(xy1) == 2 and len(xy2) == 2
    return xy1[0] * xy2[1] - xy1[1] * xy2[0]

def mdot(*args):
    """ A single consistent syntax for matrix multiplication.

    Of course, such was the point of the matrix multiplication operator,
    but for some reason numpy decided to stop supporting that for arrays
    of objects. """
    import numpy as np
    import sympy
    import operator
    from functools import reduce
    is_numpy = lambda x: isinstance(x, np.ndarray)
    is_sympy = lambda x: isinstance(x, (sympy.Matrix, sympy.ImmutableMatrix))

    if is_numpy(args[0]):
        if not all(map(is_numpy, args)):
            raise TypeError(set(map(type, args)))
        return reduce(np.dot, args)

    elif is_sympy(args[0]):
        if not all(map(is_sympy, args)):
            raise TypeError(set(map(type, args)))
        return reduce(operator.mul, args)

    else:
        raise TypeError(type(args[0]))


# Ensure that the cell is the one used on the bilbao server,
# which apparently has an interior angle of 120 degrees, not 60.
# (this distinction is important, because it affects the location of K)
def validate_bilbao_hex_cell(m):
    top, bot = m[:2], m[2:]
    assert sqnorm(top) == sqnorm(bot)
    assert dot(top, bot) / sqnorm(
        bot) == -S(1) / 2  # Note: +1/2 would imply the angle is 60


# precondition:   M has hexagonal symmetry with cell angle == 60 or 120
# postcondition:  M has hexagonal symmetry with cell angle == 120
def no_60s_allowed(M):
    M = Matrix(M)
    if dot(M[2:], M[:2]) / dot(M[2:], M[2:]) == +S(1) / 2:
        M[2] -= M[0]
        M[3] -= M[1]
    assert dot(M[2:], M[:2]) / dot(M[2:], M[2:]) == -S(1) / 2
    return M

def iterrows(M):
    for i in M.rows:
        yield list(M[i,:])

def validate_standard_hex_cell(M):
    # vectors of equal length
    assert sqnorm(M[0,:]) == sqnorm(M[1,:]), M
    # obtuse angle
    assert dot(M[0,:], M[1,:]) < 0, M
    # 120 degrees; results in the property that (a + b) is similar to a and b
    assert sqnorm(M[0,:] + M[1,:]) == sqnorm(M[0,:]), M

def do_special_case_sanity_checks(d):
    if d['key']['string'] == '1-0-1-1-1-1':
        assert d['meta']['C'] == [[1,0],[0,1]]
        assert d['meta']['D'] == [[1,0],[0,1]]
        assert d['meta']['A'] == d['meta']['B']


if __name__ == '__main__':
    main()
