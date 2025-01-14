# ABSTAT*Inf* 

[![Build Status](https://travis-ci.org/rporrini/abstat-inf.svg?branch=master)](https://travis-ci.org/rporrini/abstat-inf)

ABSTAT*Inf* is a [SWI-Prolog](http://www.swi-prolog.org) prototype capable to infer data patterns and their relative occurrence statistics in Linked Data sets. The prototype is built on top of the [ABSTAT](https://github.com/rporrini/abstat) framework.

### Run it

ABSTAT*Inf* can be easily run within a [Docker](https://www.docker.com/) container. In order to do so, clone the repo, then install [install](https://docs.docker.com/engine/installation/) Docker in your machine, open a terminal, and type ```./inference-console```.

If your are on Windows or OSX, you can use [Docker Machine](https://docs.docker.com/machine/install-machine/) to run a complete docker environment and refer to the previous command, as well as the other commands in this README. 

The above command will start a SWI-Prolog interpreter and load the [default linked data set](summaries/dbpedia-2014) (used for experiments) along with the corresponding summary from ABSTAT. You can compute the occurrence statistics of an inferred pattern via the predicate ```summary:occurrence(C, P, D, Occ)```.

![Inference Console](docs/inference-console.png?raw=true)

You may take a look at some [example goals](src/acceptance_tests.pl) to submit to the interpreter.

### Testing and Experiments

ABSTAT*Inf* comes with a suite of [unit tests](src/unit_tests.pl) to ensure the correctness of the inference algorithm. To run them issue a ```./test-console unit```.

There is also an [acceptance test](src/acceptance_tests.pl) suite, to test correctness over a real world [linked data set](summaries/dbpedia-2014), which is a portion of [DBpedia](http://dbpedia.org). To run it, type ```./test-console acceptance```.

The same data set is used for [effeciency benchmarking](src/benchmark_tests.pl) purposes. You can reproduce the benchmark by issuing a ```./test-console benchmark```.

For convenience, the ```./test-console``` command accepts a list of test suites and runs them sequentially, so that you can execute more tests with one single command, e.g., ```./test-console unit acceptance benchmark```.

### Running outside the Docker environment

If you are running Linux, you can also run ABSTAT*Inf* outside the Docker container by installing a SWI-Prolog interpreter (>=7.1.37). You can then the run the utility bash commands ```bin/infer-patterns``` and ```bin/test```, in the same way as for the above scripts for running and testing.

### Contacts
[riccardo.porrini@disco.unimib.it](mailto:riccardo.porrini@disco.unimib.it)
