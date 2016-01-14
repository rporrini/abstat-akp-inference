:- begin_tests(suite).

:- use_module(library(semweb/rdf_db)).
:- use_module(main).
:- use_module(summary).
:- load_all.

test('should compute minimal type patterns occurrences') :- 
	mPatterns('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Person', I),
	length(I, L),
	assertion( L = 61653 ).

test('should infer trivial patterns') :- 
	occurrence('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/PrimeMinister', L),
	assertion( L = 2 ).

test('should infer patterns') :- 
	occurrence('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Politician', L),
	assertion( L = 37 ).

:- end_tests(suite).

