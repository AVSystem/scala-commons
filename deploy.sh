tar xv -C travis -f travis/secrets.tar
sbt +publishSigned sonatypeRelease
