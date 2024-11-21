#!/bin/sh

if [ "$1" = "-p" ]; then
    source $2
    cmd="rust-lldb --one-line-before-file \"command script import lldb/type_summaries.py\" target/debug/journ -- ${args}"
    echo "${cmd}"
    exec rust-lldb --one-line-before-file "command script import lldb/type_summaries.py" target/debug/journ -- ${args}
else
    cmd="rust-lldb --one-line-before-file \"command script import lldb/type_summaries.py\" target/debug/journ -- $@"
    echo "${cmd}"
    exec rust-lldb --one-line-before-file "command script import lldb/type_summaries.py" target/debug/journ -- $@
fi
