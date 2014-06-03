#!/usr/bin/env python
import subprocess

p = subprocess.Popen("/home/dogue/ircdb/dist/build/ircdb/ircdb",stdout=subprocess.PIPE)
print ("Content-type: text/html\n\n")
print p.stdout.read()
p.wait()
