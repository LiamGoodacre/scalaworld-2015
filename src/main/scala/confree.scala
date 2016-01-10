import scalaz._, Scalaz._

package object confree {
  object algebra {
    sealed trait ConfigF[A]

    case class ConfigInt   [A](field: String, value: Int     => A) extends ConfigF[A]
    case class ConfigFlag  [A](field: String, value: Boolean => A) extends ConfigF[A]
    case class ConfigPort  [A](field: String, value: Int     => A) extends ConfigF[A]
    case class ConfigServer[A](field: String, value: String  => A) extends ConfigF[A]
    case class ConfigFile  [A](field: String, value: String  => A) extends ConfigF[A]
    case class ConfigSub   [A](field: String, value: FreeAp[ConfigF, A])   extends ConfigF[A]
  }

  object dsl {
    import algebra._

    type Dsl[A] = FreeAp[ConfigF, A]

    private def lift[A](value: ConfigF[A]): Dsl[A] = FreeAp.lift[ConfigF, A](value)

    def int   (field: String): Dsl[Int]     = lift(ConfigInt   (field, identity))
    def flag  (field: String): Dsl[Boolean] = lift(ConfigFlag  (field, identity))
    def port  (field: String): Dsl[Int]     = lift(ConfigPort  (field, identity))
    def server(field: String): Dsl[String]  = lift(ConfigServer(field, identity))
    def file  (field: String): Dsl[String]  = lift(ConfigFile  (field, identity))
    def sub[A](field: String) 
              (value: Dsl[A])               = lift(ConfigSub   (field, value))
  }

  object json {
    import argonaut._, Argonaut._
    import algebra._
    import dsl.Dsl

    implicit val MonadDecodeJson: Monad[DecodeJson] = new Monad[DecodeJson] {
      def point[A](a: => A): DecodeJson[A] = DecodeJson(_ => DecodeResult.ok(a))

      def bind[A, B](fa: DecodeJson[A])(f: A => DecodeJson[B]): DecodeJson[B] = fa.flatMap(f)
    }

    def genDecode[A](config: Dsl[A]): DecodeJson[A] = 
      config.foldMap(new NaturalTransformation[ConfigF, DecodeJson] {
        def apply[A](value: ConfigF[A]): DecodeJson[A] = DecodeJson(json => value match {
          case ConfigInt   (n, v) => (json --\ n).as[Int]    .map(v)
          case ConfigFlag  (n, v) => (json --\ n).as[Boolean].map(v)
          case ConfigPort  (n, v) => (json --\ n).as[Int]    .map(v)
          case ConfigServer(n, v) => (json --\ n).as[String] .map(v)
          case ConfigFile  (n, v) => (json --\ n).as[String] .map(v)
          case ConfigSub   (n, v) =>  for {
                                        sub <- (json --\ n).as[Json]
                                        a   <- genDecode(v).decodeJson(sub)
                                      } yield a
        })
      })
  }

  object help {
    import algebra._
    import dsl.Dsl

    case class HelpState(help: String = "", indent: Int = 0) {
      def --> (h: String): HelpState =
        copy(help = help + (0 until indent * 2).foldLeft[String]("")((a, _) => a + " ") + h + "\n")

      def indented: HelpState = copy(indent = indent + 1)
      def dedented: HelpState = copy(indent = indent - 1)
    }

    def genHelp[A](config: Dsl[A]): String = {
      type G[T] = State[HelpState, Const[Unit, T]]
      implicit val applicativeG = Applicative[({type L[T] = State[HelpState, T]})#L]
                                     .compose[({type L[T] = Const[Unit, T]})#L]
      def log[T](s: String): G[T] = State.modify[HelpState](_ --> s).map(Const.apply)

      def genHelp0[S](config: Dsl[S]): G[S] = {
        config.foldMap(new NaturalTransformation[ConfigF, G] {
          def apply[T](value: ConfigF[T]): G[T] = value match {
            case ConfigInt   (n, _) => log[T](n + "\t - an integer"      )
            case ConfigFlag  (n, _) => log[T](n + "\t - a boolean flag"  )
            case ConfigPort  (n, _) => log[T](n + "\t - a port number"   )
            case ConfigServer(n, _) => log[T](n + "\t - a server address")
            case ConfigFile  (n, _) => log[T](n + "\t - a file path"     )
            case ConfigSub   (n, s) => for {
              _ <- log[T](n + "\t - a sub-configuration")
              _ <- State.modify[HelpState](_.indented)
              _ <- genHelp0(s)
              _ <- State.modify[HelpState](_.dedented)
            } yield Const(())
          }
        })
      }

      genHelp0(config).exec(HelpState()).help
    }
  }

  object example {
    import argonaut._, Argonaut._
    import dsl._

    case class AuthConfig(port: Int, host: String)
    case class ServerConfig(logging: Boolean, auth: AuthConfig)

    val authConfig   = (int("port") |@| server("host"))(AuthConfig)
    val serverConfig = (flag("logging") |@| sub("auth")(authConfig))(ServerConfig)

    val serverConfigDecode = json.genDecode(serverConfig)
    val serverConfigHelp   = help.genHelp(serverConfig)

    val serverConfigJson =
      Json("logging" -> jTrue, "auth" -> Json("port" -> jNumberOrNull(2020), "host" -> jString("localhost")))

    val serverConfigDecoded = serverConfigDecode.decodeJson(serverConfigJson)
  }
}
