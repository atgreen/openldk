#!/bin/sh
#
# Copyright (C) 2026  Anthony Green <green@moxielogic.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
#
# This file is part of OpenLDK.
#
# Build a shim "JDK image" directory that runs on OpenLDK, for use as the
# jtreg "tested JDK" (jtreg's -jdk:<home>).  jtreg expects a real JDK
# directory layout; OpenLDK is a single launcher that reads a stock JDK's
# class library (lib/modules) and release metadata.  This script assembles a
# directory that *is* that stock payload -- release, lib, conf symlinked from
# a real JDK 25 -- with only bin/java and bin/javac overlaid to invoke
# OpenLDK.  JAVA_HOME inside the wrappers points at the shim itself, so the
# reported java.home and the class library actually loaded agree.
#
# Nothing is installed system-wide; the shim is a self-contained directory.
#
# Environment (all overridable):
#   OPENLDK_BIN  built openldk launcher   (default: repo-root/openldk)
#   JAVACL_BIN   built javacl executable  (default: repo-root/javacl)
#   TEMURIN      a real JDK 25 home       (default: the OpenLDK-cached Temurin)
#   SHIM         output image directory   (default: $HOME/openldk-jdk)
#
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo=$(CDPATH= cd -- "$here/../.." && pwd)

OPENLDK_BIN="${OPENLDK_BIN:-$repo/openldk}"
JAVACL_BIN="${JAVACL_BIN:-$repo/javacl}"
TEMURIN="${TEMURIN:-$HOME/.cache/openldk/jdk/jdk-25.0.4.1+1}"
SHIM="${SHIM:-$HOME/openldk-jdk}"

for req in "$OPENLDK_BIN" "$TEMURIN/release" "$TEMURIN/lib/modules"; do
    if [ ! -e "$req" ]; then
        echo "error: missing required path: $req" >&2
        echo "  (build OpenLDK with 'make openldk', or set OPENLDK_BIN/TEMURIN)" >&2
        exit 1
    fi
done

rm -rf "$SHIM"
mkdir -p "$SHIM/bin"

# Real payload OpenLDK reads: class library (lib/modules), release metadata,
# and conf/include/legal where present.  Symlinked, never copied.
for d in lib conf include release man legal; do
    [ -e "$TEMURIN/$d" ] && ln -s "$TEMURIN/$d" "$SHIM/$d"
done

# bin/java -> OpenLDK.  JAVA_HOME is the shim itself so java.home resolves to
# this image and lib/modules is found through the symlink above.
cat > "$SHIM/bin/java" <<EOF
#!/bin/sh
export JAVA_HOME="$SHIM"
exec "$OPENLDK_BIN" "\$@"
EOF
chmod +x "$SHIM/bin/java"

# bin/javac -> javacl.  Only used if you later point -jdk at the shim for
# compilation; the recommended bring-up compiles with -compilejdk:<Temurin>
# and never touches this.  Provided for completeness.
if [ -e "$JAVACL_BIN" ]; then
    cat > "$SHIM/bin/javac" <<EOF
#!/bin/sh
export JAVA_HOME="$SHIM"
exec "$JAVACL_BIN" "\$@"
EOF
    chmod +x "$SHIM/bin/javac"
else
    echo "note: $JAVACL_BIN not found; skipping bin/javac (not needed for -compilejdk runs)" >&2
fi

echo "Shim JDK image ready: $SHIM"
echo "Verifying identity:"
"$SHIM/bin/java" -version
