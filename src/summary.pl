:- module(summary, [load/1, descendant/2, descendants/2, akp/4]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

:- rdf_register_prefix(lds, 'http://ld-summaries.org/ontology/').

load(File) :- rdf_load(File, [format(ntriples), silent(true)]).

descendants(Concept, Descendants) :- 
	findall(Descendant, descendant(Descendant, Concept), DescendantList),
	list_to_set(DescendantList, DescendantSet),
	append(DescendantSet, [Concept], Descendants).

descendant(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, Superconcept).
descendant(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, X),
	descendant(X, Superconcept).

akp(Akp, Subject, Predicate, Object) :-
	rdf(Akp, rdf:subject, Subject),
	rdf(Akp, rdf:predicate, Predicate),
	rdf(Akp, rdf:object, Object).

