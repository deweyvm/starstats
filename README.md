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

`starstats` requires the following programs and libraries:
 - Python 3.X along with `pyodbc` and `bs4`.
 - A relatively up to date version of `cabal` and `ghc`, which can be installed via the  `haskell-platform` package.
 - The `highcharts` graphing library.
 - The `mysql-server` package.
 - The `unixodbc-dev` package.
 - A webserver. I used `apache2` and the installation guide will assume this is the server being used.


Setup
-----

I followed the following procedure from a fresh install of Debian 7 64bit.

    $ su root
    $ apt-get update
    $ apt-get dist-upgrade
    $ apt-get install git

#### Locale setup
    $ su root
    $ echo en_US.UTF-8 UTF-8 >> /etc/locale.gen
    $ /usr/sbin/locale-gen

#### Install mysql and odbc

I set the password here to "password" for the root user. It is for local use only so this shouldn't be an issue.

    $ apt-get install mysql-server
    $ apt-get install unixodbc-dev
    $ echo "alias mysql=\"mysql -uroot -ppassword\"" >> ~/.bash_profile

#### Install and set up python
    $ apt-get install python3 python3-dev
    $ apt-get install python3-pip
    $ pip-3.2 install pyodbc
    $ pip-3.2 install beautifulsoup4

#### Install and set up haskell
    $ apt-get install haskell-platform
    $ cabal update
    $ cabal install cabal-install
    $ mv ~/.cabal/bin/cabal /usr/bin/cabal

#### Setup mysql
    $ sudo apt-get install libmyodbc
    $ wget http://dev.mysql.com/get/Downloads/Connector-ODBC/5.3/mysql-connector-odbc-5.3.2-linux-debian6.0-x86-64bit.tar.gz
    $ tar xfv mysql-connector-odbc-5.3.2-linux-debian6.0-x86-64bit.tar.gz
    $ cd mysql-connector-odbc-5.3.2-linux-debian6.0-x86-64bit
    $ sudo mv lib/libmyodbc5* /usr/lib/x86_64-linux-gnu/odbc/
    $ cd bin
    $ sudo ln -s /usr/lib/x86_64-linux-gnu/libodbcinst.so /usr/lib/x86_64-linux-gnu/libodbcinst.so.2
    $ sudo ln -s /usr/lib/x86_64-linux-gnu/libodbc.so /usr/lib/x86_64-linux-gnu/libodbc.so.2
    $ sudo ./myodbc-installer -d -a -n "MySql ODBC 5.3 Driver" -t "DRIVER=/usr/lib/x86_64-linux-gnu/odbc/libmyodbc5a.so;SETUP=/usr/lib/x86_64-linux-gnu/odbc/libodbcmyS5.so"
    $ sudo ./myodbc-installer -s -a -c2 -n "starstats" -t "DRIVER=MySQL ODBC 5.3 Driver;SERVER=localhost;UID=root;PWD=password;"
    $ cd ~/ && rm -rf mysql-connector-odbc-5.3.2-linux-debian6.0-x86-64bit*
    $ cp /etc/odbc.ini  ~/.odbc.ini

#### Checkout the project
    $ git clone https://github.com/deweyvm/starstats.git starstats
    $ cd starstats
    $ cabal configure
Alternatively to --force-reinstalls you could just use a sandbox, but I don't bother because my machine doesnt do anything else
    $ export LANG="en_US.UTF8"
    $ cabal install --force-reinstalls


#### Setup apache (assumes apache2 already installed)
    $ sudo mv /etc/apache2/mods-available/rewrite.load /etc/apache2/mods-enabled/

I made my `enabled-sites/000-default` file look like:

        <VirtualHost *:80>
            ServerAdmin webmaster@localhost

            DocumentRoot /var/www
            <Directory />
                    Options FollowSymLinks
                    AllowOverride All
            </Directory>
            <Directory /var/www/>
                    Options Indexes FollowSymLinks MultiViews
                    AllowOverride All
                    Order allow,deny
                    allow from all
            </Directory>

            ScriptAlias /cgi-bin/ /usr/lib/cgi-bin/
            <Directory "/usr/lib/cgi-bin">
                    AllowOverride All
                    Options +ExecCGI -MultiViews +SymLinksIfOwnerMatch
                    Order allow,deny
                    AddHandler cgi-script .cgi .py
                    Allow from all
            </Directory>

            ErrorLog ${APACHE_LOG_DIR}/error.log

            # Possible values include: debug, info, notice, warn, error, crit,
            # alert, emerg.
            LogLevel warn

            CustomLog ${APACHE_LOG_DIR}/access.log combined
    </VirtualHost>

    $ /etc/init.d/apache2 restart

#### Download and install highcharts
    $ wget http://code.highcharts.com/zips/Highcharts-4.0.1.zip
    $ mkdir hc
    $ mv http://code.highcharts.com/zips/Highcharts-4.0.1.zip hc
    $ cd hc && unzip hc
    $ mv js/highcharts.js js/modules/exporting.js /var/www/starstats/

#### Setup irssi
    $ apt-get install irssi screen
    $ screen irssi
    # add channels, enable logging, etc

#### Finally, run the program
    $ bash build.sh
    $ echo "create database starstats_channelname;" | mysql
    $ dist/build/starstats/starstats -db=channelname --driver="MySql ODBC 5.3 Driver" --logtype=irssi --log=~/path/to/my.log --watch


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
