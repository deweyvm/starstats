starstats
=========

`starstats` gathers and generates statistical data from irc logs, supporting a variety of formats.

`starstats` can also generate a webpage for viewing the data and supports 
real time data access for multi-million line data logs.

In addition, a chat log may be watched in order to update the dataset in real time.

An example page may be found at [http://olivine.dogue.in/example.html]()

Installation
============

Prerequisites
-------------

Starstats requires the following programs and libraries:
 - Python 3.X along with `pyodbc` and `bs4`.
 - A relatively up to date version of `cabal` and `ghc`, which can be installed via the  `haskell-platform` package.
 - The `highcharts` graphing library.
 - The `mysql-server` package.
 - The `unixodbc-dev` package.
 - A webserver. I used `apache2` and the installation guide will assume this is the server being used.


Setup
-----

todo

License
=======
This codebase is distributed under the MIT license. It is worth noting however that `highcharts` is only free to use for noncommercial purposes.
