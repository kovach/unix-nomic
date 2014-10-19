#!/usr/bin/env bash

function header() {
    echo
    echo "###"
    echo "### $1"
    echo "###"
    echo
}

function run() {
    echo -e "# $*"
    eval "$*"
    res="$?"
    return "$res"
}

function try() {
    eval "run $@"
    res="$?"
    if [ "$res" -ne 0 ]; then
        echo "### failed ($res) ###"
        exit $res
    fi
}

header unix-nomic installation

if [ $(id -u) != "0" ]
then
    echo This script must be run as root.
    exit 1
fi

header Core tools
try apt-get update
try apt-get --yes --force-yes install \
    build-essential \
    ghc \
    python \
    python-devel \
    python-pip \
    nginx \
    node \
    vim \
    emacs \

header Building game software

header Installing game software

header Cron jobs
