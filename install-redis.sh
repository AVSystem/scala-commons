#!/usr/bin/env bash
if [ ! -d redis-$REDIS_VERSION/src ]; then
    wget http://download.redis.io/releases/redis-$REDIS_VERSION.tar.gz
    tar -xzf redis-$REDIS_VERSION.tar.gz
    cd redis-$REDIS_VERSION && make
fi
