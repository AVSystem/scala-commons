openssl aes-256-cbc -K $encrypted_b21e52cd6f46_key -iv $encrypted_b21e52cd6f46_iv -in travis/secrets.tar.enc -out travis/local.secrets.tar -d
tar xv -C travis -f travis/local.secrets.tar
sbt -Dsbt.color=true +publishSigned
SCALAJS_VERSION=0.6.33 sbt -Dsbt.color=true +commons-js/publishSigned
sbt -Dsbt.color=true sonatypeBundleRelease
