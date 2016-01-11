:- begin_tests(suite).

:- use_module(library(semweb/rdf_db)).
:- use_module(main).
:- use_module(library(statistics)).

:- 
	writeln('----------------------------------------------'),
	writeln('TRIPLE LOADING TIME'), 
	time(load_all),
	writeln('----------------------------------------------'),

	writeln('dbo:Film dbo:Director dbo:PrimeMinister'),
	time(occurrence('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/PrimeMinister', A)),
	writeln(A),
	writeln('----------------------------------------------'),
	writeln('dbo:Film dbo:Director dbo:Politician'),
	time(occurrence('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Politician', B)),
	writeln(B),
	writeln('----------------------------------------------'),
	writeln('dbo:Film dbo:Director dbo:Person'),
	time(occurrence('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Person', C)),
	writeln(C),
	writeln('----------------------------------------------'),
	writeln('dbo:Film dbo:Director dbo:Agent'),
	time(occurrence('http://dbpedia.org/ontology/Film', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Agent', D)),
	writeln(D),
	writeln('----------------------------------------------'),

	writeln('dbo:Movie dbo:Director dbo:PrimeMinister'),
	time(occurrence('http://dbpedia.org/ontology/Movie', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/PrimeMinister', E)),
	writeln(E),
	writeln('----------------------------------------------'),
	writeln('dbo:Movie dbo:Director dbo:Politician'),
	time(occurrence('http://dbpedia.org/ontology/Movie', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Politician', F)),
	writeln(F),
	writeln('----------------------------------------------'),
	writeln('dbo:Movie dbo:Director dbo:Person'),
	time(occurrence('http://dbpedia.org/ontology/Movie', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Person', G)),
	writeln(G),
	writeln('----------------------------------------------'),
	writeln('dbo:Movie dbo:Director dbo:Agent'),
	time(occurrence('http://dbpedia.org/ontology/Movie', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Agent', H)),
	writeln(H),
	writeln('----------------------------------------------').

:- end_tests(suite).

