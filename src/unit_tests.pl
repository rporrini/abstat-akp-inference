:- begin_tests(suite).

:- use_module(library(semweb/rdf_db)).
:- use_module(summary).

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

test('sublcass relations should be used to reconstruct a simple hierarchy', nondet, [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	descendant(subclass, Superclass),
	assertion(Superclass == superclass).

test('subclasses should be collected', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <b> .',
		'<b> <http://www.w3.org/2004/02/skos/core#broader> <c> .',
		'<c> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	descendants(superclass, Descendants),
	assertion(Descendants == [c,subclass,b,superclass]).

test('subclasses should include the concept itself', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<subclass> <http://www.w3.org/2004/02/skos/core#broader> <superclass> .'
	]),
	descendants(superclass, Descendants),
	assertion(Descendants == [subclass,superclass]).

test('should get the minimal type of an instance', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<instance> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <type> .'
	]),
	minimalType(instance, Type),
	assertion(Type == type).

test('should get all instancies of a given AKP', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<the_movie> <director> <ron_jeffries> .',
		'<the_movie> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <film> .',
		'<ron_jeffries> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <person> .'
	]),
	akps(film, director, person, AKPs),
	assertion(AKPs == [{the_movie, ron_jeffries}]).

test('should get all instancies of a given AKP via subject type inference', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<the_movie> <director> <ron_jeffries> .',
		'<the_movie> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <film> .',
		'<film> <http://www.w3.org/2004/02/skos/core#broader> <work> .',
		'<ron_jeffries> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <person> .'
	]),
	inferredAkps(work, director, person, AKPs),
	assertion(AKPs == [{the_movie, ron_jeffries}]).

test('should get all instancies of a given AKP via object type inference', [cleanup(empty_kb(File))]) :- 
	kb(File,[
		'<the_movie> <director> <ron_jeffries> .',
		'<the_movie> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <film> .',
		'<ron_jeffries> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <person> .',
		'<person> <http://www.w3.org/2004/02/skos/core#broader> <agent> .'
	]),
	inferredAkps(film, director, agent, AKPs),
	assertion(AKPs == [{the_movie, ron_jeffries}]).

:- end_tests(suite).

