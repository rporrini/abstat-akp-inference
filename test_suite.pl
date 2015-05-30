:- use_module(library(semweb/rdf_db)).
:- use_module(summary).

:- begin_tests(suite).

file(File, Lines) :- 
	tmp_file(test, File),
	open(File, write, Stream),
	lines(Stream, Lines),
	close(Stream).
lines(_, []) :- !.
lines(Stream, [Line|Others]) :-
	write(Stream, Line),
	nl(Stream),
	lines(Stream, Others).

test("a single triple should be loaded", [cleanup(delete_file(File))]) :- 
	file(File, [
		"<subject> <predicate> <object> ."
	]),
	load(File),
	rdf(subject, predicate, object).
test("many triples should be loaded", [cleanup(delete_file(File))]) :- 
	file(File, [
		"<1> <2> <3> .",
		"<4> <5> <6> .",
		"<7> <8> <9> ."
	]),
	load(File),
	rdf('1', '2', '3'),
	rdf('4', '5', '6'),
	rdf('7', '8', '9').
test("should always pass") :- true.

:- end_tests(suite).

