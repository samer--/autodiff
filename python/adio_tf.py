import tensorflow as tf
import sys

from adio import *

def new_var(): return
def function(in_vars, out_vars):
    gr = tf.get_default_graph()
    def run(*in_vals):
        d = dict(zip(in_vars, in_vals))
        with tf.Session(graph=gr) as sess:
            return sess.run(out_vars, d)
    return run

def replace_nones(x, ys): return [x if y is None else y for y in ys]

Ops = { 'exp':  tf.exp
      , 'log':  tf.log
      , 'grad': (lambda y, x: replace_nones(tf.constant(0.0), tf.gradients(y, x)))
      , 'new_scalar': (lambda: tf.placeholder(tf.float64, shape=()))
      , 'new_vector': (lambda n:tf.placeholder(tf.float64, shape=(n,)))
      , 'function':  function
      }

if __name__=="__main__":
    speed_test(5, inside_outside, Ops, load(sys.argv[1]))


