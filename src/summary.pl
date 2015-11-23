:- module(summary, [load/1, sub_concept/2, akp/3]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

load(File) :- 
	rdf_load(File, [format(ntriples), silent(false)]).

skos:broader(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, Superconcept).
sub_concept(Subconcept, Superconcept) :-
	skos:broader(Subconcept, Superconcept).
sub_concept(Subconcept, Superconcept) :-
	skos:broader(Subconcept, X),
	sub_concept(X, Superconcept).

akp(Subject, Predicate, Object) :-
	rdf(Akp, rdf:subject, Subject),
	rdf(Akp, rdf:predicate, Predicate),
	rdf(Akp, rdf:object, Object).
akp(Superconcept, Predicate, Object) :-
	sub_concept(Subject, Superconcept),
	akp(Subject, Predicate, Object).
akp(Subject, Predicate, Superconcept) :-
	sub_concept(Object, Superconcept),
	akp(Subject, Predicate, Object).

