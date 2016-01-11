:- begin_tests(suite).

:- use_module(library(semweb/rdf_db)).
:- use_module(main).
:- use_module(library(statistics)).

:- 
	writeln('----------------------------------------------'),
	writeln('TRIPLE LOADING TIME'), 
	time(load_all),
	writeln('----------------------------------------------'),
	writeln('dbo:Film dbo:Director PrimeMinister'),
	time(occurrence('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/PrimeMinister', O)),
	writeln(O),
	writeln('----------------------------------------------').

:- end_tests(suite).

