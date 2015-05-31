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

test('a single triple should be loaded', [cleanup(delete_file(File))]) :- 
	file(File, [
		'<subject> <predicate> <object> .'
	]),
	load(File),
	rdf(subject, predicate, object).
test('many triples should be loaded', [cleanup(delete_file(File))]) :- 
	file(File, [
		'<1> <2> <3> .',
		'<4> <5> <6> .',
		'<7> <8> <9> .'
	]),
	load(File),
	rdf('1', '2', '3'),
	rdf('4', '5', '6'),
	rdf('7', '8', '9').
test('sublcass relations should be used to reconstruct a simple hierarchy', [cleanup(delete_file(File))]) :- 
	file(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	load(File),
	sub_class('subclass', 'superclass').
test('subclassed should be queriable', [cleanup(delete_file(File))]) :- 
	file(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	load(File),
	sub_class(subclass, Superclass),
	assertion(Superclass == superclass).
test('should always pass') :- true.

:- end_tests(suite).

