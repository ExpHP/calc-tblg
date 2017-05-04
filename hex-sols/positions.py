#!/usr/bin/env python3

# Transform haskell output into less sucky json and compute positions.
# (implicit units of C-C bond length)

from moire.exact import MoirePattern, find_nicer_cell
from sympy import S, Matrix, sqrt, oo, rot_axis3, atan2, simplify

import itertools
import sys
import json


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
    parser.add_argument(
        '--shift-db',
        default='shift-db',
        help="skip computing the shift matrix, which is currently done"
        " using an absolutely retartedly inefficient brute force method")
    parser.add_argument('--paranoid', '-P', default=0, action='count')
    parser.add_argument('--carefree', '-C', default=0, action='count')
    parser.add_argument(
        '--min-volume', '-n', default=0, type=int, help='inclusive')
    parser.add_argument(
        '--max-volume', '-N', default=None, type=int, help='exclusive')
    args = parser.parse_args()

    PARANOID = args.paranoid - args.carefree

    if sys.stdin.isatty:
        print("Reading from standard input...", file=sys.stderr)

    run = lambda x: main_(x, args.shift_db, args.min_volume, args.max_volume)
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


def main_(soln, shift_db, min_volume, max_volume):
    beta, (a, b, c), rparts = soln
    rn = int(rparts['numerator'])
    rd = int(rparts['denominator'])
    rk = int(rparts['rootNum'])
    r = S(rn) / rd * sqrt(rk)
    # family matrices and sites ought to be added to input
    #  before we start trying anything but hex
    assert beta == 3

    key_layout = 'a-b-c-n-d-k'
    key_parts = [a, b, c, rn, rd, rk]
    key_string = '-'.join(map(str, map(int, key_parts)))
    key = dict(layout=key_layout, parts=key_parts, string=key_string)

    print("handling {}".format(key_string), file=sys.stderr)

    sites = [[0, 0], [S(2) / 3, S(1) / 3]]
    A = Matrix([[1, 0], [-S(1) / 2, sqrt(3) / 2]])

    M = S(r) / c * Matrix([[a, -b * sqrt(beta)], [b * sqrt(beta), a]])
    B = A * M.T

    mp = MoirePattern.from_cells(A, B)

    SC = find_nicer_cell(mp.commensurate_cell())
    # rotate/reflect basis for lammps
    (SC, trans) = lammps_friendly_cell(SC)
    SC = no_60s_allowed(SC)
    A = A * trans
    B = B * trans

    C = SC * A.inv()
    D = SC * B.inv()

    volumes = [abs(C.det()), abs(D.det())]
    if max_volume is not None and S(max_volume) <= S(volumes[0]):
        return None
    if S(volumes[0]) < S(min_volume):
        return None

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

    # Matrix describing max translations for a single layer
    # before an identical pattern is obtained

    U = lookup_or_call(shift_db, key_string, bruteForceCartShiftLattice, SC,
                       csSites, dsLatt)

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
        'shift': {
            'cart': {
                'approx': unmat(float, U),
            },
            'frac': {
                'approx': unmat(float, U @ SC.inv()),
            },
        },
        'meta': {
            'abc': [a, b, c],
            'β': beta,
            'families': [
                [[1, 2], [1, 2]],
                [[1, 2], [1, 2]],
            ],
            'A': {
                'cart': {
                    'approx': unmat(float, A)
                },
                'frac': {
                    'approx': unmat(float, C.inv())
                },
            },
            'B': {
                'cart': {
                    'approx': unmat(float, B)
                },
                'frac': {
                    'approx': unmat(float, D.inv())
                },
            },
            'C': unmat(int, C),
            'D': unmat(int, D),
            'volume': {
                # in units of...
                'A': abs(int(C.det())),
                'B': abs(int(D.det())),
            },
            'S': {
                'approx': unmat(float, SC)
            },
            'r': {
                'square': [rn * rn * rk, rd * rd],
                'exact': [rn, rd, rk],
                'approx': float(r),
            },
        },
    }


def lookup_or_call(shift_db, key, func, *args, **kw):
    import pickle
    try:
        with open(shift_db, 'rb') as f:
            db = pickle.load(f)
    except FileNotFoundError:
        db = {}

    if key not in db:
        db[key] = func(*args, **kw)
        with atomic_overwrite(shift_db, 'wb') as f:
            pickle.dump(db, f)
    return db[key]


class atomic_overwrite:
    ''' atomic as in it won't do a partial write.  STILL NOT THREADSAFE! '''

    def __init__(self, path, mode='w'):
        import random
        self.target = path
        self.tmp_path = '.tmp.' + hex(random.randrange(16**16))[2:]
        self.file = open(self.tmp_path, mode)

    def __enter__(self):
        return self.file.__enter__()

    def __exit__(self, *args):
        import os
        ex_ty, ex_val, trace = args
        if ex_ty is None:
            self.file.flush()  # ??? does file.__exit__ do this?
            os.fsync(self.file.fileno())
            self.file.__exit__(*args)

            os.rename(self.tmp_path, self.target)
        else:
            return self.file.__exit__(*args)


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
    m = (m * trans).as_mutable()

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


# Ensure that the cell is the one used on the bilbao server,
# which apparently has an interior angle of 120 degrees, not 60.
# (this distinction is important, because it affects the location of K)
def validate_bilbao_hex_cell(m):
    top, bot = m[:2], m[2:]
    assert sqnorm(top) == sqnorm(bot)
    assert dot(top, bot) / sqnorm(
        bot) == -S(1) / 2  # Note: +1/2 would imply the angle is 60


# this doesn't seem right either
def bruteForceCartShiftLattice(SC, a_idx, b_idx):
    import numpy as np

    a_idx = np.array(list(a_idx))
    b_idx = np.array(list(b_idx))

    if PARANOID >= 2:
        assert np.logical_and(0 <= a_idx, a_idx < 1).all()
        assert np.logical_and(0 <= b_idx, b_idx < 1).all()

    SC = np.array(SC.tolist())

    b_idx = np.vstack([
        b_idx + ij for ij in itertools.product([-S(1), S(0), S(1)], repeat=2)
    ])

    a_cart = a_idx @ SC
    b_cart = b_idx @ SC

    def cdisp_fast():
        def bound():
            import random
            a_list = list(a_cart)
            b_list = list(b_cart)
            tmp = []
            for _i in range(10):
                a = random.choice(a_list)
                for _i in range(10):
                    b = random.choice(b_list)
                    tmp.append(sqrt(sum((a - b)**2)))
            tmp.sort()
            tmp = tmp[::-1]
            while tmp and tmp[-1] == 0:
                tmp.pop()

            # do we take best or second best?
            # NOTE: k = 1 should work when symmetry implies that the two vectors of the
            #             primitive shift cell are of equal length.
            #       k = 2 should work in all cases
            k = 1
            if len(tmp) < k:
                return oo
            return tmp[-k]

        radius = bound()

        def ranges():
            ax = np.float32(a_cart[:, 1])
            bx = np.float32(b_cart[:, 1])
            # unstable sort
            #idx = np.argsort(bx)
            # stable sort
            #idx = np.lexsort([np.arange(bx.shape[0]), bx])
            idx = np.lexsort([np.arange(bx.shape[0]), bx])
            bx = bx[idx]
            los = np.searchsorted(bx, ax - radius, side='left')
            his = np.searchsorted(bx, ax + radius, side='right')
            out = [idx[lo:hi] for (lo, hi) in zip(los, his)]
            return out

        ranges = ranges()

        return np.vstack([b_cart[r] - xy for (r, xy) in zip(ranges, a_cart)])

    def cdisp_boring():
        return np.vstack([b_cart - xy for xy in a_cart])

    cdisp = cdisp_fast()

    # we should expect all minimum length edges in cdisp
    if PARANOID >= 2:
        actual = set(map(tuple, cdisp))
        expected_cdisp = cdisp_boring()
        expected = set(map(tuple, expected_cdisp))
        assert actual - expected == set()

        true_min = min(sqnorm(x) for x in expected)
        assert set(x for x in expected
                   if sqnorm(x) == true_min) - actual == set()

    def result(cdisp):
        # don't use linalg.norm due to sympy types, and save float to very end for precision
        cdist2 = np.float32(cdisp[:, 0]**2 + cdisp[:, 1]**2)

        order = np.argsort(cdist2)
        cdisp = cdisp[order]
        cdist2 = cdist2[order]

        skipto = np.searchsorted(cdist2, 0, side='right')
        cdisp = cdisp[skipto:]
        del cdist2

        # get a nondegenerate basis of small vectors
        def get_nondegenerate_basis():
            nonlocal mout
            nonlocal cdisp
            cdisp = cdisp[::-1].tolist()
            out1 = cdisp.pop()
            while cross2(cdisp[-1], out1) == 0:
                cdisp.pop()
            out2 = cdisp.pop()

            return Matrix([list(out1), list(out2)])

        mout = get_nondegenerate_basis()

        # every remaining disp should be a integer linear combination of these
        if PARANOID >= 1:
            mInv = np.array(mout.inv().tolist())
            assert ((cdisp @ mInv) % 1 == 0).all()

        return mout

    mout = result(cdisp)
    if PARANOID >= 2:
        m2 = result(expected_cdisp)
        assert abs(mout.det()) == abs(m2.det()), "{} {} {} {}".format(
            mout, m2, mout.det(), m2.det())

    if mout.det() < 0:
        mout.row_swap(0, 1)
    assert mout.det() > 0
    if mout[0] < 0:
        mout = -mout
    return mout


# precondition:   M has hexagonal symmetry with cell angle == 60 or 120
# postcondition:  M has hexagonal symmetry with cell angle == 120
def no_60s_allowed(M):
    M = Matrix(M)
    if dot(M[2:], M[:2]) / dot(M[2:], M[2:]) == +S(1) / 2:
        M[2] -= M[0]
        M[3] -= M[1]
    assert dot(M[2:], M[:2]) / dot(M[2:], M[2:]) == -S(1) / 2
    return M

def get_Delaunay_reduction(M, tolerance):
    extended_bases = Matrix([M[:2], M[2:], -M[:2]-M[2:]])

    for i in range(100):
        if reduce_bases(extended_bases):
            break
    else:
        raise RuntimeError("Delaunay reduction failure")

    return get_shortest_bases_from_extended_bases(extended_bases)

def reduce_bases(extended_bases):
    metric = extended_bases @ extended_bases.T
    n = metric.rows

    # for i in range(n):
    #     for j in range(i+1, n):
    #         if metric[i][j] > tolerance:
    #             for k in range(n):
    #                 if (k != i) and (k != j):
    #                     extended_bases[k] += extended_bases[i]
    #             extended_bases[i] = -extended_bases[i]
    #             extended_bases[j] = extended_bases[j]
    #             return False

    # # Reduction is completed.
    # # All non diagonal elements of metric tensor is negative.
    # return True

    for j in range(n):
        for i in range(j):
            if metric[i][j] > 0:
                for k in set(range(n)) - {i,j}:
                    extended_bases[k,:] += extended_bases[i,:]
                extended_bases[i,:] *= -1
                return False

    # Reduction is completed.
    # All non diagonal elements of metric tensor is negative.
    return True

def iterrows(M):
    for i in M.rows:
        yield list(M[i,:])

def get_shortest_bases_from_extented_bases(extended_bases):

    n = extended_bases.rows
    basis = list(iterrows(extended_bases))
    for i in range(n):
        basis.append()
    basis[:4] = extended_bases
    basis[4]  = extended_bases[0] + extended_bases[1]
    basis[5]  = extended_bases[1] + extended_bases[2]
    basis[6]  = extended_bases[2] + extended_bases[0]
    # Sort bases by the lengthes (shorter is earlier)
    basis = sorted(basis, key=lambda vec: (vec ** 2).sum())

    # Choose shortest and linearly independent three bases
    # This algorithm may not be perfect.
    for i in range(7):
        for j in range(i + 1, 7):
            for k in range(j + 1, 7):
                if abs(np.linalg.det(
                        [basis[i], basis[j], basis[k]])) > tolerance:
                    return np.array([basis[i], basis[j], basis[k]])

    print("Delaunary reduction is failed.")
    return np.array(basis[:3], dtype='double')

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
