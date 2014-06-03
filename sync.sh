cp dist/build/ircdb/ircdb /usr/lib/cgi-bin
chmod a+x /usr/lib/cgi-bin/ircdb
cp generate.cgi /usr/lib/cgi-bin
chmod a+r /usr/lib/cgi-bin/generate.cgi
chown dogue:dogue /usr/lib/cgi-bin/*
cp css.css util.js /var/www
chmod a+r /var/www/*
