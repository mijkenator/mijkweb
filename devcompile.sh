#!/bin/bash

#./rebar compile || exit 0
./rebar -C rebar.devcfg compile || exit 0

cp apps/mijkweb/ebin/* rel/mijkweb/lib/mijkweb-1/ebin/
cp apps/mijk_statist/ebin/* rel/mijkweb/lib/mijk_statist-1/ebin/
