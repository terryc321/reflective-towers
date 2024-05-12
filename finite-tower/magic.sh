#!/bin/bash

# generate dummy defines file
# so modern scheme will not complain
grep -e "^(set!" simple.scm | awk -F ' ' '{print "(define " $2 " #f)"}' > defines.scm


