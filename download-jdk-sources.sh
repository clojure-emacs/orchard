#!/bin/bash -x
URL="$1"
QUALIFIER=$2
DEST=$3

# Download JDK sources from Github and repackage in the same fashion as src.zip
# that is normally distributed with JDK.
wget "$URL" -O full-src.zip
unzip -q full-src.zip
cp -r "$QUALIFIER"u-*/src/java.base/share/classes java.base
cp -r "$QUALIFIER"u-*/src/java.desktop/share/classes java.desktop
zip -qr $DEST java.base java.desktop
rm -rf java.base java.desktop "$QUALIFIER"u- full-src.zip

# Now, copy the downloaded zip to where it is supposed to be in the JDK dir.
mkdir -p $JAVA_HOME/lib
cp $DEST $JAVA_HOME/lib/src.zip
