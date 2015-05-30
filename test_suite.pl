:- use_module(library(semweb/rdf_db)).
:- use_module(summary).

:- begin_tests(suite).

create_rdf_file(File) :- 
	tmp_file('test', File),
	open(File, write, Stream),
	write(Stream, '<subject> <predicate> <object> .'),
	nl(Stream),
	close(Stream).
test('rdf files should be loaded', [setup(create_rdf_file(File)), cleanup(delete_file(File))]) :- 
	load(File),
	rdf(subject, predicate, object).
test('should always pass') :- true.

:- end_tests(suite).

