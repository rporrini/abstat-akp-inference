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

test('sublcass relations should be used to reconstruct a simple hierarchy', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	sub_concept(subclass, superclass).
test('subclasses should be queriable', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	sub_concept(subclass, Superclass),
	assertion(Superclass == superclass).
test('subclasses should be entailed', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <b> .',
		'<b> <http://www.w3.org/2004/02/skos/core#broader> <c> .',
		'<c> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	sub_concept(subclass, superclass).
test('subclasses should be entailed', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <b> .',
		'<b> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	findall(Superclass, sub_concept(subclass, Superclass), Superclasses),
	assertion(Superclasses == [b, superclass]).

test('akps should be represented') :-
	current_predicate(akp/3).
test('an akp should be loaded as reified statement',[cleanup(empty_kb(File))]) :-
	kb(File, [
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .'
	]),
	akp(subject, predicate, object).

test('akps subjects should be entailed', [cleanup(empty_kb(File))]) :-
	kb(File, [
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .'
	]),
	findall(Concept, akp(Concept, predicate, object), Concepts),
	assertion(Concepts == [subclass, superclass]).
test('akps objects should be entailed', [cleanup(empty_kb(File))]) :-
	kb(File, [
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <subclass> .'
	]),
	findall(Concept, akp(subject, predicate, Concept), Concepts),
	assertion(Concepts == [subclass, superclass]).
test('akps subjects and objects should be entailed', [cleanup(empty_kb(File))]) :-
	kb(File, [
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subclass> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <subclass> .'
	]),
	akp(subclass, predicate, subclass),
	akp(superclass, predicate, subclass),
	akp(subclass, predicate, superclass),
	akp(superclass, predicate, superclass).
test('occurrences of akp should be tracked', [cleanup(empty_kb(File))]) :-
	kb(File, [
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .',
		'<akp> <http://ld-summaries.org/ontology/occurrence> "1"^^<http://www.w3.org/2001/XMLSchema#int> .'
	]),
	findall(Occurrence, akp_occurrence(subject, predicate, object, Occurrence), Occurrences),
	assertion(Occurrences == [1]) .
test('occurrences of akp should be inferred for the subject', [cleanup(empty_kb(File))]) :-
	kb(File, [
		'<subject> <http://www.w3.org/2004/02/skos/core#broader> <thing> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .',
		'<akp> <http://ld-summaries.org/ontology/occurrence> "1"^^<http://www.w3.org/2001/XMLSchema#int> .'
	]),
	findall(Occurrence, akp_occurrence(thing, predicate, object, Occurrence), Occurrences),
	assertion(Occurrences == [1]) .
test('occurrences of akp should be inferred for the object', [cleanup(empty_kb(File))]) :-
	kb(File, [
		'<object> <http://www.w3.org/2004/02/skos/core#broader> <thing> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .',
		'<akp> <http://ld-summaries.org/ontology/occurrence> "1"^^<http://www.w3.org/2001/XMLSchema#int> .'
	]),
	findall(Occurrence, akp_occurrence(subject, predicate, thing, Occurrence), Occurrences),
	assertion(Occurrences == [1]) .
test('occurrences of akp should be inferred for both subject and object', [cleanup(empty_kb(File))]) :-
	kb(File, [
		'<object> <http://www.w3.org/2004/02/skos/core#broader> <thing> .',
		'<subject> <http://www.w3.org/2004/02/skos/core#broader> <thing> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <subject> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <predicate> .',
		'<akp> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> <object> .',
		'<akp> <http://ld-summaries.org/ontology/occurrence> "1"^^<http://www.w3.org/2001/XMLSchema#int> .'
	]),
	findall(Occurrence, akp_occurrence(thing, predicate, thing, Occurrence), Occurrences),
	assertion(Occurrences == [1]) .

:- end_tests(suite).

