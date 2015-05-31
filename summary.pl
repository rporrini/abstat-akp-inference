:- module(summary, [load/1, sub_class/2]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

load(File) :- 
	rdf_load(File, [format(ntriples), silent(true)]).

skos:broader(Subclass, Superclass) :-
	rdf(Subclass, skos:broader, Superclass).
sub_class(Subclass, Superclass) :-
	skos:broader(Subclass, Superclass).
sub_class(Subclass, Superclass) :-
	skos:broader(Subclass, X),
	sub_class(X, Superclass).
