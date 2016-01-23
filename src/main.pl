:- module(main, [load_all/0,start_server/0]).
:- use_module(summary).
:- use_module(server).

load_all :-
	writeln("% Loading the triples"),
	load("summaries/dbpedia-2014/concept-graph-cleaned.nt"),
	load("summaries/dbpedia-2014/directors.nt"),
	load("summaries/dbpedia-2014/minimal-types.nt"),
	writeln("% Done").

start_server :- server(80).

