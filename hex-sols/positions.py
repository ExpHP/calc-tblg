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
    AM = mdot(A, M.T)
    AMM = mdot(A, M.T, M.T)

    kw = {
        'max_volume': max_volume,
        'min_volume': min_volume,
    }
    positions = {
        'ab': do_multi_layer(sites, [A, AM], **kw),
    }

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

            v = int(positions['ab']['meta']['volume'][0]) # de-sympify due to poor support for format specs
            key_parts = [v, letter]
            key_string = '{:03d}-{}'.format(*key_parts)

        else:
            raise RuntimeError("incomplete switch for Layout")

        return dict(layout=key_layout, parts=key_parts, string=key_string)

    key = compute_key()

    return {
        'key': key,
        'solution': {
            'abc': [a, b, c],
            'β': beta,
            'families': [
                [[1, 2], [1, 2]],
                [[1, 2], [1, 2]],
            ],
            'r': {
                'square': [rn * rn * rk, rd * rd],
                'exact': [rn, rd, rk],
                'approx': float(r),
            },
        },
        'positions': positions,
    }


def do_multi_layer(basicSites, units, *, max_volume, min_volume):
    from functools import reduce
    SC = reduce(
        (lambda A,B: MoirePattern.from_cells(A,B).commensurate_cell()),
        units,
    )
    SC = find_nicer_cell(SC)
    # rotate/reflect basis for lammps
    (SC, trans) = lammps_friendly_cell(SC)
    SC = no_60s_allowed(SC)

    units = [mdot(A, trans) for A in units]
    coeffs = [mdot(SC, A.inv()) for A in units]
    volumes = [abs(C.det()) for C in coeffs]
    if max_volume is not None and S(max_volume) <= S(volumes[0]):
        return None
    if S(volumes[0]) < S(min_volume):
        return None

    # FIXME:  HACK:
    # check that the cell is "standard" for hexagonal;
    # both vectors should be of equal length
    assert sqnorm(SC[:2]) == sqnorm(SC[2:])

    maxIndices = [supercellMaxIndices(C) for C in coeffs]
    # # sites in A basis or B basis (xxLatt are integer coords)
    # allLatts = [list(supercellPoints(C)) for C in coeffs]
    # allSites = [
    #     [(i + di, k + dk) for (i, k) in latts for (di, dk) in basicSites]
    #     for latts in allLatts
    # ]
    # # sites in S basis
    # allLatt = [
    #     fracModMatrixMany(C, latts)
    #     for (C, latts) in zip(coeffs, allLatts)
    # ]
    # allSites = [
    #     fracModMatrixMany(C, sites)
    #     for (C, sites) in zip(coeffs, allSites)
    # ]

    # if PARANOID >= 0:
    #     for (C, sites, latts) in zip(coeffs, allSites, allLatts):
    #         assert len(sites) == len(set(sites))
    #         assert len(latts) == C.det()
    #         assert len(sites) == C.det() * len(basicSites)

    if PARANOID >= 1:
        validate_standard_hex_cell(SC)
        for A in units:
        # for (A, sites, latts) in zip(units, allSites, allLatts):
            # validate_hexagonal_shape(SC, latts)
            # validate_honeycomb_shape(SC, sites)
            validate_standard_hex_cell(A)

    uniter = lambda f, it: [f(x) for x in it]
    uniter2 = lambda f, it: [[f(x) for x in xs] for xs in it]
    unmat = lambda f, M: uniter2(f, M.tolist())

    return {
        'lattice': unmat(float, SC),
        'layer': [
            {
                'frac-sites': uniter2(float, basicSites),
                'frac-lattice': unmat(float, C.inv()),
                'repeat': uniter(int, idx),
            } for (C, idx) in zip(coeffs, maxIndices)
        ],
        'meta': {
            'layer': [
                {
                    'cart': {'approx': unmat(float, A)},
                    'frac': {'approx': unmat(float, C.inv())},
                } for (A, C) in zip(units, coeffs)
            ],
            'coeff': [ unmat(int, C) for C in coeffs ],
            'volume': [ abs(int(C.det())) for C in coeffs ],
        },
    }


def cut_out_third_layer(d):
    # We fully specify how the dictionary should be transformed
    #  to be sure we don't miss anything.
    ACTION_KEEP = object()
    ACTION_TAKE_2_OF_3 = object()
    SPEC = {
        'lattice': ACTION_KEEP,
        'layer': ACTION_TAKE_2_OF_3,
        'meta': {
            'layer': ACTION_TAKE_2_OF_3,
            'coeff': ACTION_TAKE_2_OF_3,
            'volume': ACTION_TAKE_2_OF_3,
        },
    }

    def transform_by_spec(spec, d):
        if spec is ACTION_KEEP:
            return d
        elif spec is ACTION_TAKE_2_OF_3:
            assert isinstance(d, list)
            assert len(d) == 3
            return d[:2]
        elif isinstance(spec, dict):
            return zip_dict_with(transform_by_spec, spec, d)
        else:
            assert False, "complete switch"

    return zip_dict_with(transform_by_spec, SPEC, d)

def zip_dict_with(func, d1, d2):
    assert isinstance(d1, dict)
    assert isinstance(d2, dict)
    if set(d1) != set(d2):
        raise ValueError("dict keysets not parallel")
    return { k:func(d1[k], d2[k]) for k in d1 }

# def validate_hexagonal_shape(A, fracs):
#     fracs = [(i + di, k + dk)
#               for (i, k) in fracs
#               for (di, dk) in itertools.product([-1, 0, 1], repeat=2)]

#     carts = list(map(mulMatrix(A), fracs))
#     carts.sort(key=sqnorm)
#     carts = carts[::-1]
#     assert sqnorm(carts.pop()) == 0
#     # 6 nearest neighbors
#     u = [carts.pop() for _ in range(6)]
#     z = carts.pop()
#     assert all(sqnorm(x) == sqnorm(u[0]) for x in u), list(map(sqnorm, u))
#     assert sqnorm(u[0]) < sqnorm(z)

# def validate_honeycomb_shape(A, fracs):
#     fracs = [(i + di, k + dk)
#               for (i, k) in fracs
#               for (di, dk) in itertools.product([-1, 0], repeat=2)]
#     carts = list(map(mulMatrix(A), fracs))
#     carts.sort(key=sqnorm)
#     carts = carts[::-1]
#     assert sqnorm(carts.pop()) == 0
#     u = carts.pop()
#     v = carts.pop()
#     w = carts.pop()
#     z = carts.pop()
#     # 3 nearest neighbors
#     assert sqnorm(u) == sqnorm(v) == sqnorm(w), '{} {} {}'.format(
#         *map(sqnorm, (u, v, w)))
#     assert sqnorm(u) < sqnorm(z)
#     assert abs(dot(u, v)) == abs(dot(v, w)) == abs(dot(w, u))
#     assert abs(dot(u, v)) / (
#         norm(u) * norm(v)) == S(1) / 2, abs(dot(u, v)) / norm(u)


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


# # Input:  Integer supercell matrix
# # Output: Integer coords of unitcell lattice points in supercell
# def supercellPoints(m):
#    getfrac = fracModMatrix(m)
#    seen = set()

#    # how many points to a row? (all rows will share the same number, because the lengths
#    #  of parallel lines between two parallel lines are equal)
#    for j in itertools.count(1):
#        if getfrac((0, j)) == (0, 0):
#            nj = j
#            break

#    for i in itertools.count(0):
#        # find first new column in this row
#        for j in range(nj):
#            if getfrac((i, j)) not in seen:
#                break
#        # went a whole period; there's nothing new left
#        else:
#            break

#        ijs = [(i, j) for j in range(j, j + nj)]
#        fracs = set(getfrac(ij) for ij in ijs)
#        assert len(ijs) == len(fracs)
#        seen.update(fracs)
#        yield from ijs

# Input:  Integer supercell matrix
# Output: (iMax, jMax), the max number of unique images along each axis
#         (equivalently, the diagonal of the HNF of C)
def supercellMaxIndices(m):
    # HACK
    # I never thought this little rust thing would see the light of day (especially since it's
    # "yet another implementation" of an algorithm that shows up 50 bajillion times in this code
    # base), but when it comes to unit cells that are tens of thousands of atoms,
    # python is *just too slow*.
    from subprocess import Popen, PIPE
    p = Popen('hnf-search/target/release/hnf-search', stdin=PIPE, stdout=PIPE, stderr=PIPE)
    mInv = m.inv().tolist()
    mInv = [[x.as_numer_denom() for x in row] for row in mInv]
    mInv = [["{}/{}".format(n,d) for (n,d) in row] for row in mInv]
    mInv = '[{}]'.format(','.join('[{}]'.format(','.join(row)) for row in mInv))
    (out, err) = p.communicate(mInv.encode('utf-8'))
    import json
    [[c00,_],[_,c11]] = json.loads(out.decode('utf-8'))
    return c00,c11

    # SLOW PYTHON VERSION

    # getfrac = fracModMatrix(m)
    # seen = set()

    # # how many points to a row? (all rows will share the same number, because the lengths
    # #  of parallel lines between two parallel lines are equal)
    # for j in itertools.count(1):
    #     if getfrac((0, j)) == (0, 0):
    #         nj = j
    #         break

    # for i in itertools.count(0):
    #     # find first new column in this row
    #     for j in range(nj):
    #         if getfrac((i, j)) not in seen:
    #             break
    #     # went a whole period; there's nothing new left
    #     else:
    #         ni = i
    #         break

    #     ijs = [(i, j) for j in range(j, j + nj)]
    #     fracs = set(getfrac(ij) for ij in ijs)
    #     assert len(ijs) == len(fracs)
    #     seen.update(fracs)
    # return (ni, nj)

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


def fracModMatrixMany(m, ij):
    # One might think that, no matter how inefficient the implementation of matrix
    #  multiplication is in sympy, it could not possibly be so inefficient that there
    #  would be any tangible speedup by switching to numpy dtype=object arrays.
    #
    # ...One would, of course, be dead wrong.
    import numpy as np
    mInv = m.inv()
    mInv = np.array(mInv.tolist())
    ij = np.array(list(map(list, ij)))
    ij = mdot(ij, mInv) % 1
    return list(map(tuple, ij.tolist()))
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
        for p in d['positions']:
            for C in p['meta']['coeff']:
                assert C == [[1,0],[0,1]]
            A,*Bs = p['meta']['layer']
            for B in Bs:
                assert A == B

if __name__ == '__main__':
    main()
