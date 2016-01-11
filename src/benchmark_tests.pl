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

	writeln('dbo:Anime dbo:Director dbo:ComicsCreator'),
	time(occurrence('http://dbpedia.org/ontology/Anime', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/ComicsCreator', E)),
	writeln(E),
	writeln('----------------------------------------------'),
	writeln('dbo:Anime dbo:Director dbo:Artist'),
	time(occurrence('http://dbpedia.org/ontology/Anime', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Artist', F)),
	writeln(F),
	writeln('----------------------------------------------'),
	writeln('dbo:Anime dbo:Director dbo:Person'),
	time(occurrence('http://dbpedia.org/ontology/Anime', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Person', G)),
	writeln(G),
	writeln('----------------------------------------------'),
	writeln('dbo:Anime dbo:Director dbo:Agent'),
	time(occurrence('http://dbpedia.org/ontology/Anime', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Agent', H)),
	writeln(H),
	writeln('----------------------------------------------'),

	writeln('dbo:TelevisionEpisode dbo:Director dbo:ComicsCreator'),
	time(occurrence('http://dbpedia.org/ontology/TelevisionEpisode', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Writer', I)),
	writeln(I),
	writeln('----------------------------------------------'),
	writeln('dbo:TelevisionEpisode dbo:Director dbo:Artist'),
	time(occurrence('http://dbpedia.org/ontology/TelevisionEpisode', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Artist', L)),
	writeln(L),
	writeln('----------------------------------------------'),
	writeln('dbo:TelevisionEpisode dbo:Director dbo:Person'),
	time(occurrence('http://dbpedia.org/ontology/TelevisionEpisode', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Person', M)),
	writeln(M),
	writeln('----------------------------------------------'),
	writeln('dbo:TelevisionEpisode dbo:Director dbo:Agent'),
	time(occurrence('http://dbpedia.org/ontology/TelevisionEpisode', 'http://dbpedia.org/ontology/director', 'http://dbpedia.org/ontology/Agent', N)),
	writeln(N),
	writeln('----------------------------------------------').

:- end_tests(suite).

