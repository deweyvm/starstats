cp -a dist/build/ircdb/ircdb /usr/lib/cgi-bin
chmod a+x /usr/lib/cgi-bin/ircdb
cp -a generate.cgi /usr/lib/cgi-bin
chmod a+r /usr/lib/cgi-bin/generate.cgi
cp -a css.css util.js /var/www
chmod a+r /var/www/*
