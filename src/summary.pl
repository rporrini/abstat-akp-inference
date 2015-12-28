:- module(summary, [load/1, descendant/2, descendants/2, minimalType/2, akps/4, inferredAkps/4]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

load(File) :- rdf_load(File, [format(ntriples)]).

descendants(Concept, Descendants) :- 
	findall(Descendant, descendant(Descendant, Concept), DescendantList),
	list_to_set(DescendantList, DescendantSet),
	append(DescendantSet, [Concept], Descendants).

descendant(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, Superconcept).
descendant(Subconcept, Superconcept) :-
	rdf(Subconcept, skos:broader, X),
	descendant(X, Superconcept).

minimalType(Entity, Type) :-
	rdf(Entity, rdf:type, Type).

akps(SubjectType, Property, ObjectType, AKPs) :-
	findall(AKP, akp(SubjectType, Property, ObjectType, AKP), AKPs).

akp(SubjectType, Property, ObjectType, AKP) :-
	rdf(Subject, Property, Object),
	minimalType(Subject, SubjectType),
	minimalType(Object, ObjectType),
	AKP = {Subject, Object}.

inferredAkps(SubjectType, Property, ObjectType, AKPs):-
	findall(AKP, inferredAkp(SubjectType, Property, ObjectType, AKP), AKPList),
	list_to_set(AKPList, AKPs).

inferredAkp(SubjectType, Property, ObjectType, AKP):-
	descendants(SubjectType, InferredSubjectTypes),
	member(InferredSubjectType, InferredSubjectTypes),
	descendants(ObjectType, InferredObjectTypes),
	member(InferredObjectType, InferredObjectTypes),
	akp(InferredSubjectType, Property, InferredObjectType, AKP).
