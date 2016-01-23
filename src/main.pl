:- module(main, [load_all/0]).
:- use_module(summary).

load_all :-
	writeln("% Loading the triples"),
	load("summaries/dbpedia-2014/concept-graph-cleaned.nt"),
	load("summaries/dbpedia-2014/directors.nt"),
	load("summaries/dbpedia-2014/minimal-types.nt"),
	load("summaries/dbpedia-2014/director-patterns.nt"),
	writeln("% Done").

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).

:- http_handler(root(.), home, []).
home(_) :-
	html_resource('https://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js',[]),
	html_resource('https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js',[]),
	html_resource('https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css',[]),
	reply_html_page(
		[
			meta([charset('utf-8')]),
			meta(['http-equiv'('X-UA-Compatible'), content('IE=edge')]),
			meta([name('viewport'), content('width=device-width, initial-scale=1')]),
			\html_requires('https://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js'),
			\html_requires('https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css'),
			title('ABSTATInf')
		],
		[
		div(['ng-app'('abstat-inf'), 'ng-controller'('count'), class('container-fluid')], 
				[
					div([class('page-header')], [h1(['Abstat', i('Inf'), ' ', small('web interface')])]),
					h4('Count occurrences of inferred Abstract Knowledge Patterns'),
					div([class('row')], 
						[div([class('col-md-4')],
							form([],[
								div([class('form-group')], [
										input([class('form-control'), type('text'), placeholder('Subject'), 'ng-model'('subject')])
									]),
								div([class('form-group')], [
										input([class('form-control'), type('text'), placeholder('Predicate'), 'ng-model'('predicate')])
									]),
								div([class('form-group')], [
										input([class('form-control'), type('text'), placeholder('Object'), 'ng-model'('object')])
									]),
								button([type('submit'), class('btn btn-primary'), 'ng-click'('count_patterns()'), 'ng-disabled'('counting_patterns')], ['Count ', span([class('glyphicon glyphicon-repeat'), 'aria-hidden'(true), 'ng-show'('counting_patterns')],[])]), ' ',
								button([type('submit'), class('btn btn-default'), 'ng-click'('get_sample()')], ['Sample query'])
								])),
						div([class('col-md-2')], [
								div([class('panel panel-default')],[
									div([class('panel-heading')], ['Occurrences']),
									div([class('panel-body')], ['{{result}}'])
								])
								
							])
						]
					)
				]
		),
		script(type('text/javascript'), "
				var abstat = angular.module('abstat-inf',[]);
				abstat.controller('count', function ($scope, $http){
					$scope.counting_patterns = false;
					$scope.count_patterns = function(){
						$scope.counting_patterns = true;
						$scope.result = 'Counting';
						$http.get('/count', {
							method: 'GET',
							params: {
								subject: $scope.subject,
								predicate: $scope.predicate,
								object: $scope.object
							}}				
						).success(function(result){
							$scope.result = result.occurrence;
							$scope.counting_patterns = false;
						}).error(function(result){
							$scope.result = 'Error';
							$scope.counting_patterns = false;
					})};
					$scope.get_sample = function(){
						$scope.subject = 'http://dbpedia.org/ontology/Film';
						$scope.predicate = 'http://dbpedia.org/ontology/director';
						$scope.object = 'http://dbpedia.org/ontology/Politician';
						$scope.count_patterns();
					};
				});				
			")
		]
	).

:- http_handler(root(count), count_patterns, []).
count_patterns(Request) :- 
	http_parameters(Request, 
		[subject(S, []), object(O, []), predicate(P, [])]
	),
	occurrence(S,P,O,C),
	reply_json(json([subject=S,predicate=P,subject=O,occurrence=C])).

server(Port) :- http_server(http_dispatch, [port(Port)]).


