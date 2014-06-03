#!/usr/bin/env python
import subprocess
env = {'MYSQL_UNIX_PORT':'/var/run/mysqld/mysqld.sock'}
p = subprocess.Popen(["/home/dogue/ircdb/dist/build/ircdb/ircdb", "MySql ODBC 5.1 Driver", "-g"],
                     stdout=subprocess.PIPE,
                     env=env)
print ("Content-type: text/html\n\n")
print p.stdout.read()
p.wait()
