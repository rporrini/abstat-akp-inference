# ABSTAT*Inf* 

[![Build Status](https://travis-ci.org/rporrini/abstat-akp-inference.svg?branch=master)](https://travis-ci.org/rporrini/abstat-akp-inference)

ABSTAT*Inf* is a [SWI-Prolog](http://www.swi-prolog.org) prototype capable to infer data patterns and their relative occurrence statistics in Linked Data sets. The prototype is built on top of the [ABSTAT](https://github.com/rporrini/abstat) framework.

### How to run it

ABSTAT*Inf* can be easily run within a [Docker](https://www.docker.com/) container. In order to do so, [install](https://docs.docker.com/engine/installation/) Docker in your machine, open a terminal, and type

```
./inference-console
```

If your are on Windows or OSX, you can use [Docker Machine](https://docs.docker.com/machine/install-machine/) to run a complete docker environment and refer to the previous command, as well as the other commands in this README. 

The above command will start a SWI-Prolog interpreter and load the [default linked data set](summaries/dbpedia-2014) (used for experiments) along with the corresponding summary from ABSTAT.

### Contacts
[riccardo.porrini@disco.unimib.it](mailto:riccardo.porrini@disco.unimib.it)
