## Run

Build Literate Haskell blog posts:

```sh
just release-build
```

Build latex diagrams:

```sh
just latex
```

Run locally with live reloading:

```sh
gem install bundler:1.17.3
bundle install
bundle exec jekyll serve --livereload
```

## Troubleshooting

> SSL_connect returned=1 errno=0 state=SSLv3 read server certificate B: certificate verify failed

[Solution](https://github.com/jekyll/jekyll/issues/3985#issuecomment-294266874):

* Download <https://curl.se/ca/cacert.pem>
* Add a environment variable SSL_CERT_FILE values /path/to/cacert.pem
