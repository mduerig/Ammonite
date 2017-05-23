package ammonite.main

/**
  * Give Ammonite the ability to read (linux) system proxy environment variables
  * and convert them into java proxy properties. Which allows Ammonite to work
  * through proxy automatically, instead of setting `System.properties` manually.
  *
  * Parameter pattern:
  * https://docs.oracle.com/javase/7/docs/api/java/net/doc-files/net-properties.html
  */
object ProxyFromEnv {
  private lazy val KeyPattern ="""([\w\d]+)_proxy""".r
  private lazy val UrlPattern ="""([\w\d]+://)?(.+@)?([\w\d\.]+):(\d+)/?""".r

  /**
    * Get current proxy environment variables.
    */
  private def getEnvs =
    sys.env.collect{ case (k, v) if k.endsWith("proxy") => (k.toLowerCase, v.toLowerCase) }

  /**
    * Convert single proxy environment variable to corresponding system proxy properties.
    */
  private def envToProps(env: (String, String)): Map[String, String] = env match {
    case ("no_proxy", noProxySeq) =>
      val converted = noProxySeq.split(""",""").mkString("|")
      //https uses the same as http's. Ftp need not to be set here.
      Map("http.nonProxyHosts" -> converted)

    case (KeyPattern(proto), UrlPattern(_, cred, host, port)) =>
      val propHost = s"$proto.proxyHost" -> host
      val propPort = s"$proto.proxyPort" -> port
      val propCred =
        if (cred == null || cred.isEmpty) Nil
        else {
          val credPair = cred.dropRight(1).split(":")
          val propUser = s"$proto.proxyUser" -> credPair.head
          val propPassword = credPair.drop(1).map(s"$proto.proxyPassword" -> _)
          Seq(propUser) ++ propPassword
        }

      (Seq(propHost, propPort) ++ propCred).toMap
    case bad => Map.empty
  }


  /**
    * Set system proxy properties from environment variables.
    * Existing properties will not be overwritten.
    */
  def setPropProxyFromEnv(envs: Map[String, String] = this.getEnvs): Unit = {
    val proxyProps = envs
      .flatMap(envToProps)
      .filter(p => !sys.props.exists(sp => sp._1 == p._1))

    sys.props ++= proxyProps
  }

}
