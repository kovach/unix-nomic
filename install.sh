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


header motd

run rm /etc/update-motd.d/*
try cp motd/* /etc/update-motd.d/


header skel directory

try cp -r skel/* /etc/skel/


header nginx

run mkdir -m 755 /var/www
try cp -r www/* /var/www

cp nginx/nginxconf /etc/nginx/sites-available/nginxconf
run rm /etc/nginx/sites-enabled/*
try ln -s /etc/nginx/{sites-available,sites-enabled}/nginxconf

service nginx restart


header /usr/local/src/core-nomic

[ -d /usr/local/src/nomic ] && try rm -rf /usr/local/src/nomic
run mkdir -m 755 /usr/local/src/nomic
try git checkout-index -a -f --prefix=/usr/local/src/nomic/unix-nomic/
