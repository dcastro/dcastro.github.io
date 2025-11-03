
# Just list all recipes by default
default:
  just --list

run:
  bundle exec jekyll serve --livereload --config _config.yml,_config_dev.yml
