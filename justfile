# Just list all recipes by default
default:
    just --list

run:
    ./scripts/run_jekyll.sh

# Watch for changes made to the blog posts written using Literate Haskell, and compile them to Markdown
watch-build:
    watchexec --clear --exts lhs -- ./scripts/run_pandoc.sh

# Run all the checks and compile the blog posts to Markdown
release-build:
    just blog-src/release-checks
    ./scripts/run_pandoc.sh
    # Check that all the links in the generated Markdown files are valid
    xrefcheck

# Build latex diagrams. Install `pdflatex` and `pdf2svg` first.
latex:
    (cd img/diagrams && pdflatex -shell-escape -interaction=nonstopmode kind-system.tex)
