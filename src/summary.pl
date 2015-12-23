:- module(summary, [load/1, sub_concept/2, akp/3, akp_occurrence/4, superconcept/2,path/2]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

:- rdf_register_prefix(lds, 'http://ld-summaries.org/ontology/').

load(File) :- 
	rdf_load(File, [format(ntriples), silent(true)]).


superconcept(Concept, 'http://www.w3.org/2002/07/owl#Thing') :- \+ rdf(Concept, skos:broader, _), !.
superconcept(Concept, Superconcept) :- rdf(Concept, skos:broader, Superconcept).

path(Concept,Path) :- path1(Concept,['http://www.w3.org/2002/07/owl#Thing'],Path).

path1(Concept,[Concept|Rest],[Concept|Rest]).
path1(Concept,[Y|Rest],Path) :- superconcept(Y,Superconcept), path1(Concept,[Superconcept,Y|Rest],Path).

path(G,A,B,P) :- path1(G,A,[B],P).

path1(_,A,[A|P1],[A|P1]).
path1(G,A,[Y|P1],P) :- 
   adjacent(X,Y,G), \+ memberchk(X,[Y|P1]), path1(G,A,[X,Y|P1],P).



sub_concept(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, Superconcept).
sub_concept(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, X),
	sub_concept(X, Superconcept),
	!.

akp_definition(Akp, Subject, Predicate, Object) :-
	rdf(Akp, rdf:subject, Subject),
	rdf(Akp, rdf:predicate, Predicate),
	rdf(Akp, rdf:object, Object).

akp(Subject, Predicate, Object) :-
	akp_definition(_, Subject, Predicate, Object).
akp(Superconcept, Predicate, Object) :-
	sub_concept(Subject, Superconcept),
	akp(Subject, Predicate, Object),
	!.
akp(Subject, Predicate, Superconcept) :-
	sub_concept(Object, Superconcept),
	akp(Subject, Predicate, Object),
	!.

akp_occurrence_definition(Subject, Predicate, Object, Occurrence) :-
	akp_definition(Akp, Subject, Predicate, Object),
	rdf(Akp, lds:occurrence, literal(type(xsd:int, Literal))),
	atom_number(Literal, Occurrence).

akp_occurrence(Subject, Predicate, Object, Occurrence) :-
	akp_occurrence_definition(Subject, Predicate, Object, Occurrence).
akp_occurrence(Superconcept, Predicate, Object, Occurrences) :-
	sub_concept(Subject, Superconcept),
	akp_occurrence(Subject, Predicate, Object, Occurrences),
	! .
akp_occurrence(Subject, Predicate, Superconcept, Occurrences) :-
	sub_concept(Object, Superconcept),
	akp_occurrence(Subject, Predicate, Object, Occurrences),
	! .
