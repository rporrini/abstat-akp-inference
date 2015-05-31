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
empty_kb(File) :-
	delete_file(File),
	rdf_retractall(_,_,_).

test('a single triple should be loaded', [cleanup(empty_kb(File))]) :- 
	file(File, [
		'<subject> <predicate> <object> .'
	]),
	load(File),
	rdf(subject, predicate, object).
test('many triples should be loaded', [cleanup(empty_kb(File))]) :- 
	file(File, [
		'<a> <b> <c> .',
		'<d> <e> <f> .',
		'<g> <h> <i> .'
	]),
	load(File),
	rdf(a, b, c),
	rdf(d, e, f),
	rdf(g, h, i).
test('sublcass relations should be used to reconstruct a simple hierarchy', [cleanup(empty_kb(File))]) :- 
	file(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	load(File),
	sub_class(subclass, superclass).
test('subclasses should be queriable', [cleanup(empty_kb(File))]) :- 
	file(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	load(File),
	sub_class(subclass, Superclass),
	assertion(Superclass == superclass).
test('subclasses should be entailed', [cleanup(empty_kb(File))]) :- 
	file(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <b> .',
		'<b> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	load(File),
	sub_class(subclass, superclass).
test('should always pass') :- true.

:- end_tests(suite).

