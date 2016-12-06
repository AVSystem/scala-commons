#!/usr/bin/env bash
if [ ! -d $REDIS_HOME ]; then
    wget https://github.com/antirez/redis/archive/$REDIS_VERSION.tar.gz
    tar -xzf $REDIS_VERSION.tar.gz
    cd redis-$REDIS_VERSION && make
fi
