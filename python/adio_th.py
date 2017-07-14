import theano
import theano.tensor as T

from adio import *

Ops = { 'exp':  T.exp
      , 'log':  T.log
      , 'grad': (lambda y, x: T.grad(y,x,disconnected_inputs='ignore'))
      , 'new_scalar': T.dscalar
      , 'new_vector': (lambda _:T.dvector())
      , 'function': theano.function
      }
