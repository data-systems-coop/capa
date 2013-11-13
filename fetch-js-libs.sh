#/bin/sh

echo "Downloading JS libs"
#root js folder
cd control/js

rm -rf jquery
mkdir jquery
wget -q -P jquery 'http://code.jquery.com/jquery.js'

rm -rf jquery.form
mkdir jquery.form
wget -q -P jquery.form 'http://malsup.github.io/jquery.form.js'

rm -rf bootstrap
mkdir bootstrap
wget -q -P bootstrap 'http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/js/bootstrap.min.js'
wget -q -P bootstrap 'http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/css/bootstrap.min.css'

rm -rf bootstrap-datepicker
mkdir bootstrap-datepicker
wget -q -P bootstrap-datepicker 'http://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.1.3/js/bootstrap-datepicker.min.js'
wget -q -P bootstrap-datepicker 'http://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.1.3/css/bootstrap-datepicker.min.css'

rm -rf sprintf
mkdir sprintf
wget -q -P sprintf 'https://raw.github.com/alexei/sprintf.js/master/src/sprintf.min.js'

rm -rf purl
mkdir purl
wget -q -P purl 'http://cdnjs.cloudflare.com/ajax/libs/jquery-url-parser/2.3.1/purl.min.js'

rm -rf jquery.mask
mkdir jquery.mask
wget -q -P jquery.mask 'http://cdnjs.cloudflare.com/ajax/libs/jquery.mask/0.9.0/jquery.mask.min.js'