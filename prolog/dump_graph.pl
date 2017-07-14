:- use_module(library(http/json)).
:- use_module(library(callutils)).
:- use_module(library(listutils), [cons/3]).
:- use_module(library(fileutils), [with_output_to_file/2]).
:- use_module(library(data/pair)).
:- use_module(library(ccprism/graph), [graph_switches/2, semiring_graph_fold/4]).

:- consult(pack(ccprism/examples/test)).

ccp_graph:sr_inj(id(P), F, _ ,F1) :- call(P,F,F1).
ccp_graph:sr_proj(id(P), G, S, S, G1) :- call(P,G,G1).
ccp_graph:sr_plus(id(_),X) --> cons(X).
ccp_graph:sr_times(id(_),X) --> cons(X).
ccp_graph:sr_zero(id(_),[]).
ccp_graph:sr_unit(id(_),[]).

dataset_graph(N,G) :-
   ccp_test:dataset(N,X), 
   ccp_test:goal_graph(maplist(phrase(s),X),G).

unmodule(_:H,H1) :- term_string(H,H1).
unmodule((_:N):=V,[N1,V]) :- term_string(N,N1).

write_graph(G,File) :-
   semiring_graph_fold(id(unmodule),G,_,G1),
   maplist(ffst(unmodule), G1, G2),
   call(maplist(unmodule&sw_values)*graph_switches, G, SWs),
   with_output_to_file(File, json_write(current_output, json([switches-json(SWs), graph-json(G2)]))).

sw_values(SW,Vals) :- call(SW,_,Vals,[]).
