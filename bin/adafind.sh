#!/bin/bash

cd ~/ada/node/domain-types/src
/bin/bash ~/bin/gfind $*
cd ~/ada/node/admin-api/src
/bin/bash ~/bin/gfind $*
cd ~/ada/node/admin-web/src
/bin/bash ~/bin/gfind $*
