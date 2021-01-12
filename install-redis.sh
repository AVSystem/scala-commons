#!/usr/bin/env bash
REDIS_HOME=$PWD/redis-$REDIS_VERSION/src
echo "REDIS_HOME=$REDIS_HOME" >> $GITHUB_ENV
if [ ! -d $REDIS_HOME ]; then
    wget http://download.redis.io/releases/redis-$REDIS_VERSION.tar.gz
    tar -xzf redis-$REDIS_VERSION.tar.gz
    cd redis-$REDIS_VERSION && make BUILD_TLS=yes
fi

