#!/usr/bin/env bash
if [ ! -d ./redis-$REDIS_VERSION ]; then
    wget http://download.redis.io/releases/redis-$REDIS_VERSION.tar.gz
    tar -xzf redis-$REDIS_VERSION.tar.gz
    cd redis-$REDIS_VERSION && make BUILD_TLS=yes
fi
echo "REDIS_HOME=./redis-$REDIS_VERSION/src" >> $GITHUB_ENV
