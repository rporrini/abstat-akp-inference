:- use_module(library(semweb/rdf_db)).
:- use_module(summary).

:- begin_tests(suite).

file(File, Lines) :- 
	tmp_file(test, File),
	open(File, write, Stream),
	line(Stream, Lines),
	close(Stream).
line(Stream, [Line|_]) :-
	write(Stream, Line),
	nl(Stream).

test('an rdf file with a single line should be loaded', [cleanup(delete_file(File))]) :- 
	file(File, ['<subject> <predicate> <object> .']),
	load(File),
	rdf(subject, predicate, object).

test('should always pass') :- true.

:- end_tests(suite).

