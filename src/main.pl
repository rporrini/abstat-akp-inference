:- use_module(summary).

load_all :-
	writeln("Loading files"),
	load("summaries/dbpedia-2014/concept-graph.nt"),
	load("summaries/dbpedia-2014/akp-datatype.nt"),
	load("summaries/dbpedia-2014/akp-object-00.nt"),
	load("summaries/dbpedia-2014/akp-object-01.nt"),
	load("summaries/dbpedia-2014/akp-object-02.nt"),
	load("summaries/dbpedia-2014/akp-object-03.nt"),
	load("summaries/dbpedia-2014/akp-object-04.nt"),
	writeln("Done").

demo :-
	akp('http://ld-summaries.org/resource/dbpedia-2014/dbpedia.org/ontology/Work', 'http://ld-summaries.org/resource/dbpedia-2014/datatype-property/xmlns.com/foaf/0.1/name', 'http://ld-summaries.org/resource/dbpedia-2014/www.w3.org/2000/01/rdf-schema#Literal'),
	writeln("dbo:Work foaf:name rdfs:Literal found using inference on the AKP base.").

