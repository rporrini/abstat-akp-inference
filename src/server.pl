:- module(server, [server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(summary).

server(Port) :- http_server(http_dispatch, [port(Port)]).

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
						[div([class('col-md-3')],
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
								button([type('submit'), class('btn btn-default'), 'ng-click'('get_sample()')], ['Sample query']), ' ',
								button([type('submit'), class('btn btn-default'), 'ng-click'('undo()')], ['Back'])
								])),
						div([class('col-md-2')], [
								div([class('panel panel-default')],[
									div([class('panel-heading')], ['Query result']),
									div([class('panel-body')], [div([], [span([class('badge')], ['{{result}}']), span(['ng-show'('stats')], [' occurrences'])]),
												    div(['ng-show'('stats')],[strong('Statistics')]),
											            div(['ng-show'('stats')],['Elapsed time: ', code('{{stats.elapsed_time | truncate }} s')]),
												    div(['ng-show'('stats')],['KB Assertions: ', code('{{stats.triples}}')]),
											 	    div(['ng-show'('stats')],['KB Instances: ', code('{{stats.entities}}')])
									])
								])
								
							]),
						div([class('col-md-3')], [
								div([class('panel panel-default')],[
									div([class('panel-heading')], ['Subject Descentants']),
									div([class('panel-body')], [
										ul([class('list-unstyled')], [li(['ng-repeat'('type in descendants.subject')],[
											a([href('#'), 'ng-click'('update_subject(type)')], ['{{type}}'])
										])])
									])
								])
								
							]),
						div([class('col-md-3')], [
								div([class('panel panel-default')],[
									div([class('panel-heading')], ['Object Descentants']),
									div([class('panel-body')], [
										ul([class('list-unstyled')], [li(['ng-repeat'('type in descendants.object')],[
											a([href('#'), 'ng-click'('update_object(type)')], ['{{type}}'])
										])])
									])
								])
								
							])
						]
					)
				]
		),
		script(type('text/javascript'), "
				var abstat = angular.module('abstat-inf',[]);
				abstat.filter('truncate', function(){
					return function(number){
						if(number) return (number).toFixed(4);
						return '';
					};
				});
				abstat.controller('count', function ($scope, $http){
					$scope.counting_patterns = false;
					$scope.descendants = [];
					$scope.history = [];
					$scope.update_subject = function(type){
						$scope.subject = type;
						$scope.count_patterns();
					};
					$scope.update_object = function(type){
						$scope.object = type;
						$scope.count_patterns();
					};
					$scope.undo = function(){
						$scope.history.pop();
						var pattern  = $scope.history.pop();
						$scope.subject = pattern['subject'];
						$scope.predicate = pattern['predicate'];
						$scope.object = pattern['object'];
						$scope.count_patterns();
					};
					$scope.count_patterns = function(){
						var pattern = {
								subject: $scope.subject,
								predicate: $scope.predicate,
								object: $scope.object
							};
						$scope.stats = null;
						$scope.counting_patterns = true;
						$scope.result = 'Counting';
						$scope.descendants.subject = ['Retrieving'];
						$scope.descendants.object = ['Retrieving'];
						$http.get('/count', {
							method: 'GET',
							params: pattern}				
						).success(function(result){
							$scope.result = result.occurrence;
							$scope.stats = result;
							$scope.counting_patterns = false;
						}).error(function(result){
							$scope.stats = null;
							$scope.result = 'Error';
							$scope.counting_patterns = false;
						});

						$http.get('/descendants', {
							method: 'GET',
							params: {
								type: $scope.subject
							}}				
						).success(function(result){
							$scope.descendants.subject = result;
						}).error(function(result){
							$scope.descendants.subject = ['Error'];
						});

						$http.get('/descendants', {
							method: 'GET',
							params: {
								type: $scope.object
							}}				
						).success(function(result){
							$scope.descendants.object = result;
						}).error(function(result){
							$scope.descendants.object = ['Error'];
						});
						$scope.history.push(pattern);
					};

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
	get_time(Start),
	occurrence(S,P,O,C),
	get_time(End),
	Delta is End - Start,
	rdf_statistics(triples(Triples)),
	rdf_statistics(resources(Entities)),
	reply_json(json([subject=S,predicate=P,subject=O,occurrence=C,triples=Triples,entities=Entities,elapsed_time=Delta])).

:- http_handler(root(descendants), get_descendants, []).
get_descendants(Request) :- 
	http_parameters(Request, 
		[type(T, [])]
	),
	descendants(T, Desc),
	prolog_to_json(Desc, Response),
	reply_json(Response).

