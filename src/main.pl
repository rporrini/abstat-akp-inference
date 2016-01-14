:- module(main, [load_all/0]).
:- use_module(summary).

load_all :-
	writeln("% Loading the triples"),
	load("summaries/dbpedia-2014/concept-graph-cleaned.nt"),
	load("summaries/dbpedia-2014/directors.nt"),
	load("summaries/dbpedia-2014/minimal-types.nt"),
	load("summaries/dbpedia-2014/director-patterns.nt"),
	writeln("% Done").


