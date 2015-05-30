:- module(summary, [load/1]).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_ntriples)).

load(File) :- 
	rdf_load(File, [format(ntriples)]).
