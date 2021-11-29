#!/bin/bash
ADA_DIR=~/ada

## Not sure what this is...
ON_START='env PROMPT_COMMAND='"'"'unset PROMPT_COMMAND; eval $('$ADA_DIR'/env.sh);'"'"' bash';

## Run docker;
cd "$ADA_DIR";
docker-compose up -d;
# sh env.sh

## Launch terminal with tabs
xfce4-terminal -H --role=docker --title="docker" --maximize --working-directory="$ADA_DIR/" -e "$ON_START" \
 --tab -H --role=admin-api  --title="admin-api"  --working-directory="$ADA_DIR/node/admin-api/" -e "$ON_START" \
 --tab -H --role=admin-web  --title="admin-web"  --working-directory="$ADA_DIR/node/admin-web/" -e "$ON_START" \
 --tab -H --role=portal-web --title="portal-web" --working-directory="$ADA_DIR/node/portal-web/" -e "$ON_START" \
 --tab -H --role=portal-api --title="portal-api" --working-directory="$ADA_DIR/java/";

## Rerun docker to fire up containers that exited first time (this
## always happens)
sleep 30;
docker-compose up -d;
