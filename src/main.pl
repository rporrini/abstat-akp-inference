:- use_module(summary).

main :-
	writeln("loading files"),
	load("summaries/dbpedia-2014/concept-graph.nt"),
	load("summaries/dbpedia-2014/akp-datatype.nt"),
	load("summaries/dbpedia-2014/akp-object-00.nt"),
	load("summaries/dbpedia-2014/akp-object-01.nt"),
	load("summaries/dbpedia-2014/akp-object-02.nt"),
	load("summaries/dbpedia-2014/akp-object-03.nt"),
	load("summaries/dbpedia-2014/akp-object-04.nt"),
	writeln("done"),
	writeln("starting writing"),
	open("file.txt", write, Stream, [buffer(line)]),
	forall(akp(X, _ ,_), write(Stream, X)),
	close(Stream).

