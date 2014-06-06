NAME=starstats
cp -a dist/build/$NAME/$NAME /usr/lib/cgi-bin
chmod a+x /usr/lib/cgi-bin/$NAME
cp -a generate.cgi /usr/lib/cgi-bin
chmod a+r /usr/lib/cgi-bin/generate.cgi
chmod a+x /usr/lib/cgi-bin/generate.cgi
cp -a css.css util.js /var/www
chmod a+r /var/www/{util.js,css.css}
