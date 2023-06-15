#!/bin/bash

# based on https://github.com/redis/redis/blob/unstable/utils/gen-test-certs.sh
# Generate some test certificates which are used by the regression test suite:
#
#   ca.{crt,key}          Self signed CA certificate.
#   redis.{crt,key}       A certificate with no key usage/policy restrictions.

generate_cert() {
    local name=$1
    local cn="$2"
    local opts="$3"

    local keyfile=${name}.key
    local certfile=${name}.crt

    [ -f $keyfile ] || openssl genrsa -out $keyfile 2048
    openssl req \
        -new -sha256 \
        -subj "/O=Redis Test/CN=$cn" \
        -key $keyfile | \
        openssl x509 \
            -req -sha256 \
            -CA ca.crt \
            -CAkey ca.key \
            -CAserial ca.txt \
            -CAcreateserial \
            -days 365 \
            $opts \
            -out $certfile
}

[ -f ca.key ] || openssl genrsa -out ca.key 4096
openssl req \
    -x509 -new -nodes -sha256 \
    -key ca.key \
    -days 3650 \
    -subj '/O=Redis Test/CN=Certificate Authority' \
    -out ca.crt

generate_cert redis "Generic-cert"

openssl pkcs12 -export -in redis.crt -inkey redis.key -out redis.p12 -passout pass:
