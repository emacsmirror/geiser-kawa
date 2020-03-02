#!/bin/env sh

# This file is not used by geiser-kawa. It's just to show that you can
# start a Kawa repl with geiser support using the Kawa version that is
# included in the maven dependencies of kawa-geiser.

# valid exec.args are either:
# - a port number: starts a Kawa telnet server listening on that port
# - --no-server: starts a Kawa repl in current terminal

mvn compile &&
    mvn exec:java \
	-D"exec.mainClass"="kawageiser.StartKawaWithGeiserSupport" \
	-D"exec.args"=$@
