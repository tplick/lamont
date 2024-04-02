#!/bin/bash
DIR=$(dirname $BASH_SOURCE[0])
if [[ "$OSTYPE" == "darwin"* ]]
then
    DYLD_FALLBACK_LIBRARY_PATH="$DIR" "$DIR/exe.opt" $@
else
    LD_LIBRARY_PATH="$DIR" "$DIR/exe.opt" $@
fi

