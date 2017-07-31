import random
import math
import numpy as np

NLAYER = 3

def mutate(*args, **kw):
    return weighted(
        (80, lambda: mutate_pos(*args, **kw)),
        (20, lambda: mutate_lattice(*args, **kw)),
    )()

def mutate_pos(carts, supercell, **kw):
    natom = supercell.primitive_size()
    assert natom % NLAYER == 0
    per_layer = natom // NLAYER

    def layer():
        k = random.randrange(NLAYER)
        return list(range(k*per_layer, (k+1)*per_layer))
    def atom():
        return [random.randrange(0, natom)]

    indices = weighted(
        (3, layer),
        (2, atom),
    )()
    _indices = indices
    indices = extend_with_images(indices, carts.shape[0], 6)

    shift_mask = weighted(
        (3, [1,1,0]),
        (3, [0,0,1]),
        (1, [1,1,1]),
    )

    radius = log_norm(-7, 3, base=10)

    print("shift_mask:", shift_mask, end='  ')
    print("indices:", _indices, end='  ')
    print("log10(radius): ", '{:4g}'.format(math.log(radius)/math.log(10)), end='  ')
    print()

    carts[indices] += random_dir(shift_mask) * radius
    return ('carts', carts)

def mutate_lattice(lattice, **kw):
    r = 1 + sign() * log_norm(-7, 3, base=10)
    lattice[0] *= r
    lattice[1] *= r
    print("scale lattice:", r)
    return ('lattice', lattice)

def sign(): return random.choice([1, -1])
def log_norm(mean, sigma, base=math.e): return random.lognormvariate(mean * math.log(base), sigma)
def extend_with_images(idxs, ntot, nunique): return [x for x0 in idxs for x in range(x0, ntot, nunique)]
def normalize(x): return x / np.linalg.norm(x)
def random_dir(mask=[1,1,1]): return normalize(np.random.normal(0, 1, (3,)) * mask)

def _weighted(*items):
    import bisect

    weights, values = zip(*items)

    cumprobs = [weights[0]]
    for w in weights[1:]:
        cumprobs.append(cumprobs[-1] + w)

    def inner():
        r = random.random() * cumprobs[-1]
        i = bisect.bisect_left(cumprobs, r)
        return values[i]
    return inner

def weighted(*items): return _weighted(*items)()
