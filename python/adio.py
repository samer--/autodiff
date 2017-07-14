import operator as op
import numpy as np
import time

from collections import namedtuple
from utils import memoise, identity, json_read
from math import log, exp

def load(f): return json_read(f)

# ----------- INSIDE ALGORITHM -------------
Semiring = namedtuple('Semiring', ['inject','project','times','plus','unit','zero'])

def uniform(n): return n*[-log(n)]

def inside_outside(ops, system):
    switches = system['switches']
    prob, params = time_call("Inside", semiring_graph_fold_p,
                             ops['new_scalar'],
                             Semiring(ops['exp'], identity, op.mul, op.add, 1.0, 0.0),
                             system['graph'],
                             switches)
    logprob    = ops['log'](prob)
    all_params = [p for (_,ps) in params for p in ps]
    grads      = time_call("Gradients", ops['grad'], logprob, all_params)
    inner_fun  = time_call("Compiling", ops['function'], all_params, [logprob]+grads)

    sw_param_vals = [(sw,uniform(len(switches[sw]))) for (sw,_) in params]
    all_param_vals = [p for (_,ps) in sw_param_vals for p in ps]

    def outer_fun(p):
        outs = inner_fun(*p)
        return outs[0], outs[1:]

    return outer_fun, all_param_vals

def inside_outside_vec(ops, system):
    switches = system['switches']
    prob, params = time_call("Inside", semiring_graph_fold_v,
                             ops['new_vector'],
                             Semiring(ops['exp'], identity, op.mul, op.add, 1.0, 0.0),
                             system['graph'], switches)
    logprob    = ops['log'](prob)
    all_params = [p for (_,p) in params]
    grads     = time_call("Gradients", ops['grad'], logprob, all_params)
    inner_fun = time_call("Compiling", ops['function'], all_params, [logprob]+grads)

    sw_param_vals = [np.array(uniform(len(switches[sw]))) for (sw,_) in params]

    def outer_fun(p):
        outs = inner_fun(*p)
        return outs[0], outs[1:]

    return outer_fun, sw_param_vals


def semiring_graph_fold_p(new_scalar, sr, graph, switches_values):
    pmap = semiring_graph_folder(lambda _:new_scalar(), sr, graph, switches_values)
    return pmap("'$top$'"), [(sw, [pmap((sw,val)) for val in vals])
                             for sw, vals in switches_values.iteritems()]


def semiring_graph_fold_v(new_vector, sr, graph, switches_values):
    @memoise
    def vmap(sw): return new_vector(len(switches_values))
    def sw_val_param((sw,val)): return vmap(sw)[switches_values[sw].index(val)]
    pmap = semiring_graph_folder(sw_val_param, sr, graph, switches_values)
    return pmap("'$top$'"), [(sw, vmap(sw)) for sw in switches_values]


def semiring_graph_folder(new_param, sr, graph, switches_values):
    @memoise
    def pmap(factor):
        return (new_param(factor) if type(factor) is tuple
                else sr.project(reduce(sr_add_prod, graph[factor], sr.zero)))

    def sr_add_prod(s1, expl): return sr.plus(s1, reduce(sr_factor, expl, sr.unit))
    def sr_factor(p1, factor): return sr.times(p1, pfactor(factor))
    def pfactor(f):            return sr.inject(pmap((f[0],f[1]))) if type(f) is list else pmap(f)

    return pmap

# ------------- timing ------------------

def speed_test(reps, io, ops, system):
    upd, s0 = time_call("Inside-outside setup", io, ops, system)
    def repeat_upd():
        for i in xrange(reps): upd(s0)
    time_call("Update (x%d)" % reps, repeat_upd)

def time_call(text, f, *args):
    t0 = time.time(); x = f(*args)
    t1 = time.time()
    print "%s: elapsed time=%f seconds" % (text, (t1-t0))
    return x

