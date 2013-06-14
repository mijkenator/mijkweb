#!/bin/bash

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)
CONFIG_DIR="${RUNNER_SCRIPT_DIR}/rel/mijkweb/releases/1/"

if [ -e "${CONFIG_DIR}vm.args" ]; then
    VMARGS_PATH="${CONFIG_DIR}vm.args"
else
    echo "No vm.args"
    exit 1
fi

if [ -e "${CONFIG_DIR}sys.config" ]; then
    CONFIG_PATH="${CONFIG_DIR}sys.config"
else
    echo "No sys.config"
    exit 2
fi

NAME_ARG=`egrep '^-s?name' $VMARGS_PATH`
if [ -z "$NAME_ARG" ]; then
    echo "vm.args needs to have either -name or -sname parameter."
    exit 3
fi

COOKIE_ARG=`grep '^-setcookie' $VMARGS_PATH`
if [ -z "$COOKIE_ARG" ]; then
    echo "vm.args needs to have a -setcookie parameter."
    exit 4
fi

COMMAND="${RUNNER_SCRIPT_DIR}/cron_daily_stats ${NAME_ARG} ${COOKIE_ARG} $1"
echo "COMMAND -> ${COMMAND}"
${COMMAND}
