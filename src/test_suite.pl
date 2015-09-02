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
kb(File, Assertions) :-
	file(File, Assertions),
	load(File).

test('a trivial test should always pass') :- true.

test('a single triple should be loaded', [cleanup(empty_kb(File))]) :- 
	kb(File, [
		'<subject> <predicate> <object> .'
	]),
	rdf(subject, predicate, object).
test('many triples should be loaded', [cleanup(empty_kb(File))]) :- 
	kb(File, [
		'<a> <b> <c> .',
		'<d> <e> <f> .',
		'<g> <h> <i> .'
	]),
	rdf(a, b, c),
	rdf(d, e, f),
	rdf(g, h, i).

test('sublcass relations should be used to reconstruct a simple hierarchy', [nondet, cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	sub_concept(subclass, superclass).
test('subclasses should be queriable', [nondet, cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	sub_concept(subclass, Superclass),
	assertion(Superclass == superclass).
test('subclasses should be entailed', [nondet, cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <b> .',
		'<b> <http://www.w3.org/2004/02/skos/core#broader> <c> .',
		'<c> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	sub_concept(subclass, superclass).
test('subclasses should be entailed', [nondet, cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <b> .',
		'<b> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	findall(Superclass, sub_concept(subclass, Superclass), Superclasses),
	assertion(Superclasses == [b, superclass]).

test('akps should be represented') :-
	current_predicate(akp/3).
test('an akp should be loaded as reified statement',[nondet, cleanup(empty_kb(File))]) :-
	kb(File, [
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .'
	]),
	akp(subject, predicate, object).

test('akps subjects should be entailed', [nondet, cleanup(empty_kb(File))]) :-
	kb(File, [
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .'
	]),
	findall(Concept, akp(Concept, predicate, object), Concepts),
	assertion(Concepts == [subclass, superclass]).
test('akps objects should be entailed', [nondet, cleanup(empty_kb(File))]) :-
	kb(File, [
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <subclass> .'
	]),
	findall(Concept, akp(subject, predicate, Concept), Concepts),
	assertion(Concepts == [subclass, superclass]).

:- end_tests(suite).

