## Install

```plain
> gem install jekyll
> bundle install
> bundle exec jekyll serve --livereload
```

To compile the latex diagrams, install pdflatex and then:

```plain
> brew install pdf2svg
> cd img/diagrams
> pdflatex -shell-escape -interaction=nonstopmode kind-system.tex
```

## Issues

> SSL_connect returned=1 errno=0 state=SSLv3 read server certificate B: certificate verify failed

[Solution](https://github.com/jekyll/jekyll/issues/3985#issuecomment-294266874):

* Download <https://curl.haxx.se/ca/cacert.pem>
* Add a environment variable SSL_CERT_FILE values /path/to/cacert.pem
