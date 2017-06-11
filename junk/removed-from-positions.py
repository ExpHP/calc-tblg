
# Max Shifts
# Horrible horrible code that attempts to compute the max amount
#   one layer can be shifted from the origin before it produces a
#   similar moire pattern.


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

    a_cart = mdot(a_idx, SC)
    b_cart = mdot(b_idx, SC)

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
            assert (mdot(cdisp, mInv) % 1 == 0).all()

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


