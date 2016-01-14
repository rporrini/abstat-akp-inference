:- module(summary, [load/1, descendant/2, descendants/2, minimalType/2, mPatterns/4, iPatterns/4, occurrence/4]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

load(File) :- rdf_load(File, [format(ntriples)]).

descendants(Type, Descendants) :- 
	findall(Descendant, descendant(Descendant, Type), DescendantList),
	list_to_set(DescendantList, DescendantSet),
	append(DescendantSet, [Type], Descendants).

descendant(Subtype, Supertype) :-
	rdf(Subtype, skos:broader, Supertype).
descendant(Subtype, Supertype) :-
	rdf(Subtype, skos:broader, X),
	descendant(X, Supertype).

minimalType(Entity, Type) :-
	rdf(Entity, rdf:type, Type).

mPatterns(C, P, D, Instances) :-
	findall(Instance, mPattern(C, P, D, Instance), Instances).

mPattern(C, P, D, Instance) :-
	rdf(Subject, P, Object),
	minimalType(Subject, C),
	minimalType(Object, D),
	Instance = {Subject, Object}.

iPatterns(C, P, D, Instances):-
	findall(Instance, iPattern(C, P, D, Instance), InstancesList),
	list_to_set(InstancesList, Instances).

iPattern(C, P, D, Instance):-
	descendants(C, ICs),
	descendants(D, IDs),
	mPattern(IC, P, ID, Instance),
	member(IC, ICs),
	member(ID, IDs).

occurrence(C, P, D, O) :-
	iPatterns(C, P, D, Patterns),
	length(Patterns, O).

