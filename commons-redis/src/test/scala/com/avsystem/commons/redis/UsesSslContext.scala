package com.avsystem.commons
package redis

import java.io.FileInputStream
import java.security.{KeyStore, SecureRandom}

import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

trait UsesSslContext {
  lazy val sslContext: SSLContext = SSLContext.getInstance("TLSv1.2").setup { sslc =>
    val ks = KeyStore.getInstance("PKCS12")
    ks.load(new FileInputStream("./tls/redis.p12"), Array.empty)

    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    kmf.init(ks, Array.empty)

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(ks)

    sslc.init(kmf.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
  }
}
