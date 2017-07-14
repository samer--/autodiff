using ReverseDiff: GradientConfig, GradientTape, gradient!, gradient, compile
using JSON


function load(f)
	open(f, "r") do h
	  return JSON.parse(readstring(h))
	end
end

function memoise(f)
	d = Dict()
	function g(x)
		if haskey(d,x)
			return d[x]
		else
			y=f(x)
			d[x]=y
			return y
		end
	end
	return g
end

# ----------- INSIDE ALGORITHM -------------
type Semiring{A,B}
	inject::Function
	project::Function
	times::Function
	plus::Function
	unit::A
	zero::B
end

uniform(n::Integer) = fill(-log(n), n)

#= function inside_outside(ops, system)x: =#
#=     switches = system['switches'] =#
#=     prob, params = time_call("Inside", semiring_graph_fold_p, =#
#=                              ops['new_scalar'], =#
#=                              Semiring(ops['exp'], identity, op.mul, op.add, 1.0, 0.0), =#
#=                              system['graph'], =#
#=                              switches) =#
#=     logprob    = ops['log'](prob) =#
#=     all_params = [p for (_,ps) in params for p in ps] =#
#=     grads      = time_call("Gradients", ops['grad'], logprob, all_params) =#
#=     inner_fun  = time_call("Compiling", ops['function'], all_params, [logprob]+grads) =#

#=     sw_param_vals = [(sw,uniform(len(switches[sw]))) for (sw,_) in params] =#
#=     all_param_vals = [p for (_,ps) in sw_param_vals for p in ps] =#

#=     return inner_fun, all_param_vals =#
#= end =#

#= function inside_outside_vec(ops, system) =#
#=     switches = system['switches'] =#
#=     prob, params = time_call("Inside", semiring_graph_fold_v, =#
#=                              ops['new_vector'], =#
#=                              Semiring(ops['exp'], identity, op.mul, op.add, 1.0, 0.0), =#
#=                              system['graph'], switches) =#
#=     logprob    = ops['log'](prob) =#
#=     all_params = [p for (_,p) in params] =#
#=     grads     = time_call("Gradients", ops['grad'], logprob, all_params) =#
#=     inner_fun = time_call("Compiling", ops['function'], all_params, [logprob]+grads) =#

#=     sw_param_vals = [np.array(uniform(len(switches[sw]))) for (sw,_) in params] =#
#=     return outer_fun, sw_param_vals =#
#= end =#

params(G) = Dict((k,v)=>(-log(length(vs))) for (k,vs) in G["switches"] for v in vs)
function semiring_graph_fold_v(sr::Semiring, graph::Dict{String, Any}, keys, vals)
	#= params = Dict(sw=>uniform(length(v)) for (k,v) in switches_values) =#
	#= params = Dict(k=>Dict(v=>(1/length(vs)) for v in vs) for (k,vs) in G["switches"]) =#
	params = Dict(zip(keys,vals))
	get_param(sw_val) = params[sw_val] 
   pmap = semiring_graph_folder(get_param, sr, graph)
   return pmap("'\$top\$'")
end

										 #= switches_values::Dict{String, Array{Any,1}}) =#

function semiring_graph_folder(get_param::Function, sr::Semiring, graph::Dict{String, Any}) 
	pmap0(factor::String) = sr.project(foldl(sr_add_prod, sr.zero, graph[factor]))
	pmap0(factor::Tuple{String,Any}) = get_param(factor)
	pmap = memoise(pmap0)

	sr_add_prod(s1, expl) = sr.plus(s1, foldl(sr_factor, sr.unit, expl))
	sr_factor(p1, factor) = sr.times(p1, pfactor(factor))
	pfactor(f::String)       = pmap(f)
	pfactor(f::Array{Any,1}) = sr.inject(pmap((f[1],f[2])))

   return pmap
end

# ------------- timing ------------------


#= Tape setup 26 ms =#
#= Compile 270 ms =#
#= Run uncompiled tape 0.23ms =#
#= run compiled tape compile 0.15ms =#
#= Run without setup 35ms =#

function speed_test2(system)
   pp = params(system)
	kk = keys(pp)
	vv = collect(values(pp))
	logprob(v) = log(semiring_graph_fold_v(Semiring(exp, identity, *, +, 1.0, 0.0),
	                                       system["graph"], kk, v))

	println("Create tape")
   tape = @time GradientTape(logprob, vv)
	println("Compile tape")
	ctape = @time compile(tape)
	res = similar(vv)

	println("Eval raw tape (x100)")
	@time for i in 1:100; gradient!(res, tape, vv); end
	println("Eval compiled tape (x100)")
	@time for i in 1:100; gradient!(res, ctape, vv); end
end

function speed_test(system)
   pp = params(system)
	kk = keys(pp)
	vv = collect(values(pp))
	logprob(v) = log(semiring_graph_fold_v(Semiring(exp, identity, *, +, 1.0, 0.0),
	                                       system["graph"], kk, v))
	println("Eval (x100)")
	@time for i in 1:100; gradient(logprob, vv); end
	gradient(logprob, vv)
end
#=     upd, s0 = time_call("Inside-outside setup", io, ops, system) =#
#=     function repeat_upd() =#
#=         for i in xrange(reps): upd(s0) =#
#= 	end =#
#=     time_call("Update (x%d)" % reps, repeat_upd) =#
#= end =#

#= function time_call(text, f, *args) =#
#=     t0 = time.time(); x = f(*args) =#
#=     t1 = time.time() =#
#=     print "%s: elapsed time=%f seconds" % (text, (t1-t0)) =#
#=     return x =#
#= end =#


