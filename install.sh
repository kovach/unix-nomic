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

echo This script installs unix-nomic on Ubuntu 14.04
echo

if [ $(id -u) != "0" ]
then
    echo This script must be run as root.
    exit 1
fi

header Core tools
try apt-get --yes --force-yes install \
    passwd \
    members \
    build-essential \
    ghc \
    python \
    python-dev \
    python-pip \
    nginx \
    node \
    vim \
    emacs \

header Installing game software

run mkdir -m 755 /nomic
run mkdir -m 755 /nomic/proposals

try cd propose
try make install
try cd ..

header Cron jobs


header skel directory

try cp -r skel/* /etc/skel/
run mkdir /etc/skel/www
try chmod a+r /etc/skel/www

header nginx

cp nginx/nginxconfig /etc/nginx/sites-available/nginxconfig
run rm /etc/nginx/sites-enabled/*
try ln -s /etc/nginx/{sites-available,sites-enabled}/nginxconfig

service nginx restart
