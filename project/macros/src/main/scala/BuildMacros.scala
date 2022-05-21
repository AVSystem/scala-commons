import scala.reflect.macros.blackbox

class BuildMacros(val c: blackbox.Context) {

  import c.universe._

  def discoverProjectsImpl: Tree = {
    val sbtProjectCls = c.mirror.staticClass("sbt.Project")
    val ptpe = c.prefix.actualType

    val projectRefs =
      ptpe.members.iterator
        .filter { m =>
          m.isTerm && m.asTerm.isGetter &&
            m.typeSignature.finalResultType.typeSymbol == sbtProjectCls
        }
        .map(m => q"${c.prefix}.$m")
        .toList

    q"_root_.scala.Seq(..$projectRefs).filter(_ != ${c.prefix}.rootProject)"
  }
}
