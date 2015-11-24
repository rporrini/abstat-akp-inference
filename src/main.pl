:- use_module(summary).

load_all :-
	writeln("Loading the summary"),
	load("summaries/dbpedia-2014/concept-graph.nt"),
	load("summaries/dbpedia-2014/akp-datatype.nt"),
	load("summaries/dbpedia-2014/akp-object-00.nt"),
	load("summaries/dbpedia-2014/akp-object-01.nt"),
	load("summaries/dbpedia-2014/akp-object-02.nt"),
	load("summaries/dbpedia-2014/akp-object-03.nt"),
	load("summaries/dbpedia-2014/akp-object-04.nt"),
	writeln("Done").

