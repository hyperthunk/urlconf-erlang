#!/usr/bin/env sh

# generate a release - because make doesn't like multiline strings
# and I don't like escaping all the time.

APPNAME=$1
APPVERSION=$2
ERTS_VERSION=$3

echo "
{release, 
{\"$APPNAME\", \"$APPVERSION\"}, 
{erts, \"$ERTS_VERSION\"},
  [{Application, AppVsn} |
 {Application, AppVsn, Type} |
 {Application, AppVsn, IncApps} |
 {Application, AppVsn, Type, IncApps}]}.
"

