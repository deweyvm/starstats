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

At least ~1GB of memory is needed for compiling the binary. If this is an issue, simply compile it on another machine and copy it over.

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


Operation
=========

`starstats` has three main modes of operation: insert, watch, and generate.

### Insert

In this mode, a file's contents will be parsed and inserted into the database.

### Watch

In this mode, a file's contents will be parsed and inserted into the database. The given file will be watched for new additions and lines added as they are added to the log.

### Generate

In this mode, a webpage is generated based on information in the database. If one chooses to use the provided CGI script, this is handled automatically.

### Other Usage

For other usage, see the --help option of the main executable.

License
=======
This codebase is distributed under the MIT license. It is worth noting however that `highcharts` is only free to use for noncommercial purposes.
