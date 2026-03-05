#!/bin/bash -i

# NOTE: This script may be run from within vscode, in which case `~/.bashrc` will be loaded in non-interactive mode.
# Most `~/.bashrc` scripts will not do anything when NOT run in interactive mode
# See: https://github.com/microsoft/vscode/issues/29412#issuecomment-311389626
#
# This means that the PATH will not be updated and ruby version managers like `asdf` will not have a chance to set the correct ruby version.
# To work around this, we configure this script to always run in interactive mode with:
#   `#!/bin/bash -i`


# Additionally, vscode terminates tasks by sending SIGHUP.
# https://github.com/microsoft/vscode/issues/206607#issuecomment-2941275558
# But bash doesn't seem to propagate SIGHUP to the children process,
# so we have to handle signals here and forward them to `bundle exec`

set -e # exit on error
set -u # error on undefined var
set -o pipefail # exit on command pipe failure

# Set working dir to script location
cd "$(dirname "$0")"

# Start bundle process in background and capture its PID
cd ..
bundle exec jekyll serve --livereload --config _config.yml,_config_dev.yml &
BUNDLE_PID=$!

# Trap and forward all signals to bundle process
for sig in HUP INT QUIT TERM USR1 USR2 PIPE ALRM TSTP TTIN TTOU WINCH CHLD; do
	trap "kill -$sig $BUNDLE_PID" $sig
done

# Wait for bundle process to finish
wait $BUNDLE_PID
