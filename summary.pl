:- module(summary, [load/1, sub_class/2]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

load(File) :- 
	rdf_load(File, [format(ntriples), silent(true)]).

sub_class(Subclass, Superclass) :- 
	rdf(Subclass, 'http://www.w3.org/2004/02/skos/core#broader', Superclass).
