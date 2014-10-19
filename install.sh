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
try apt-get update
try apt-get --yes --force-yes install \
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

try mkdir -m 755 /nomic
try mkdir -m 755 /nomic/proposals

try cd proposal
try make install
try cd ..

header Cron jobs


header skel directory

try cp -r skel/* /etc/skel/
try mkdir /etc/skel/www
try chmod a+r /etc/skel/www

header nginx

cat > /etc/nginx/sites-available/nginxconfig <<EOF
server {
  listen 80;
  # Serve user www directories
  location ~ ^/~(.+?)(/.*?)$ {
    alias /home/$1/www$2;
    index index.html index.htm;
    autoindex on;
  }
}
EOF

run rm /etc/nginx/sites-enabled/*
try ln -s /etc/nginx/{sites-available,sites-enabled}/nginxconfig

service nginx restart
