#!/bin/bash

start () {
    if [[ $(pgrep Rserve) ]]; then
        echo "Rserve already running"
    else
        R CMD ~/R/x86_64-pc-linux-gnu-library/3.5/Rserve/libs/Rserve \
            --RS-workdir . \
            --RS-socket wena-rserve \
            --RS-source rserve-load-data.R
    fi
}

stop () {
    pkill Rserve
    while [[ $(pgrep Rserve) ]]; do
        sleep 1
    done
    rm -f wena-rserve
}

restart () {
    stop
    start
}

status () {
    if [[ $(pgrep Rserve) ]]; then
        echo "Rserve already running"
    else
        echo "Rserve is not running"
    fi
}

if [[ "$1" =~ ^(start|stop|restart|status)$ ]]; then
    $1
else
    echo "Usage: ./rserve.sh [start | stop | restart | status]"
fi
