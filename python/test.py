from adio import inside_outside, speed_test, load
import adio_tf
import adio_th
import sys

ops_dict = { 'theano': adio_th.Ops, 'tf': adio_tf.Ops }

# Usage: python test.py {tf|theano} <num_iters> <graph json file>
# NB, set environment variable THEANO_FLAGS='mode=FAST_COMPILE' to speed up theano a lot
def main(argv):
    ops = ops_dict[argv[1]]
    reps = int(argv[2])
    filename = argv[3]
    print '\n -- FIRST RUN ---\n'
    speed_test(reps, inside_outside, ops, load(filename))
    print '\n -- SECOND RUN ---\n'
    speed_test(reps, inside_outside, ops, load(filename))

if __name__ == '__main__': main(sys.argv)
