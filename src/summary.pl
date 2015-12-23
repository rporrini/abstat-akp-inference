:- module(summary, [load/1, sub_concept/2, akp/3, akp_occurrence/4]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

:- rdf_register_prefix(lds, 'http://ld-summaries.org/ontology/').

load(File) :- 
	rdf_load(File, [format(ntriples), silent(true)]).

skos:broader(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, Superconcept).
sub_concept(Subconcept, Superconcept) :-
	skos:broader(Subconcept, Superconcept).
sub_concept(Subconcept, Superconcept) :-
	skos:broader(Subconcept, X),
	sub_concept(X, Superconcept).

akp_definition(Akp, Subject, Predicate, Object) :-
	rdf(Akp, rdf:subject, Subject),
	rdf(Akp, rdf:predicate, Predicate),
	rdf(Akp, rdf:object, Object).

akp(Subject, Predicate, Object) :-
	akp_definition(_, Subject, Predicate, Object).
akp(Superconcept, Predicate, Object) :-
	sub_concept(Subject, Superconcept),
	akp(Subject, Predicate, Object).
akp(Subject, Predicate, Superconcept) :-
	sub_concept(Object, Superconcept),
	akp(Subject, Predicate, Object).

akp_occurrence(Subject, Predicate, Object, Occurrence) :-
	akp_definition(Akp, Subject, Predicate, Object),
	rdf(Akp, lds:occurrence, literal(type(xsd:int, Literal))),
	atom_number(Literal, Occurrence).


