package com.avsystem.commons
package spring

import java.lang.reflect.{Constructor, Executable, Method, Modifier}

import org.springframework.core.{JdkVersion, ParameterNameDiscoverer}

import scala.annotation.tailrec
import scala.ref.WeakReference
import scala.reflect.api.JavaUniverse
import scala.reflect.{ScalaLongSignature, ScalaSignature}

object ScalaParameterNameDiscoverer {
  final val ScalaSignatureClasses =
    List(classOf[ScalaSignature], classOf[ScalaLongSignature])

  final val JdkAtLeast8 =
    JdkVersion.getMajorJavaVersion >= JdkVersion.JAVA_18

  // we don't want to keep the universe in memory forever, so we don't use scala.reflect.runtime.universe
  private var universeRef: WeakReference[JavaUniverse] = _

  private def universe: JavaUniverse = {
    universeRef.option.flatMap(_.get) match {
      case Some(result) => result
      case None =>
        val result = new scala.reflect.runtime.JavaUniverse
        universeRef = new WeakReference[JavaUniverse](result)
        result
    }
  }
}

@deprecated("this class is useless on JDK >= 1.8, use StandardReflectionParameterNameDiscoverer", "1.46.3")
class ScalaParameterNameDiscoverer extends ParameterNameDiscoverer {

  import ScalaParameterNameDiscoverer._

  @tailrec private def isScala(cls: Class[_]): Boolean = cls.getEnclosingClass match {
    case null => ScalaSignatureClasses.exists(ac => cls.getAnnotation(ac) != null)
    case encls => isScala(encls)
  }

  private def discoverNames(u: JavaUniverse)(executable: Executable, symbolPredicate: u.Symbol => Boolean): Array[String] = {
    import u._

    val declaringClass = executable.getDeclaringClass
    val mirror = runtimeMirror(declaringClass.getClassLoader)
    val ownerSymbol =
      if (Modifier.isStatic(executable.getModifiers)) mirror.moduleSymbol(declaringClass).moduleClass.asType
      else mirror.classSymbol(declaringClass)

    def argErasuresMatch(ms: MethodSymbol) =
      ms.paramLists.flatten.map(s => mirror.runtimeClass(s.typeSignature)) == executable.getParameterTypes.toList

    def paramNames(ms: MethodSymbol) =
      ms.paramLists.flatten.map(_.name.toString).toArray

    ownerSymbol.toType.members
      .find(s => symbolPredicate(s) && argErasuresMatch(s.asMethod))
      .map(s => paramNames(s.asMethod))
      .orNull
  }

  def getParameterNames(ctor: Constructor[_]): Array[String] =
    if (JdkAtLeast8 && ctor.getParameters.forall(_.isNamePresent))
      ctor.getParameters.map(_.getName)
    else if (isScala(ctor.getDeclaringClass))
      discoverNames(universe)(ctor, s => s.isConstructor)
    else null

  def getParameterNames(method: Method): Array[String] = {
    val declaringCls = method.getDeclaringClass
    if (JdkAtLeast8 && method.getParameters.forall(_.isNamePresent))
      method.getParameters.map(_.getName)
    else if (isScala(declaringCls)) {
      // https://github.com/scala/bug/issues/10650
      val forStaticForwarder =
        if (Modifier.isStatic(method.getModifiers))
          Class.forName(declaringCls.getName + "$", false, declaringCls.getClassLoader)
            .recoverToOpt[ClassNotFoundException]
            .flatMap(_.getMethod(method.getName, method.getParameterTypes: _*).recoverToOpt[NoSuchMethodException])
            .map(getParameterNames)
        else
          Opt.Empty
      forStaticForwarder.getOrElse(
        discoverNames(universe)(method, s => s.isMethod && s.name.toString == method.getName))
    }
    else null
  }
}
