#!/usr/bin/env bash
REDIS_VERSION=$1
if [ ! -d ./redis-$REDIS_VERSION/src ]; then
    wget http://download.redis.io/releases/redis-$REDIS_VERSION.tar.gz
    tar -xzf redis-$REDIS_VERSION.tar.gz
    cd redis-$REDIS_VERSION && make BUILD_TLS=yes
fi
