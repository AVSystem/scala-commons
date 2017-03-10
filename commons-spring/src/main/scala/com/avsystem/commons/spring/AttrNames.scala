package com.avsystem.commons
package spring

import com.github.ghik.silencer.silent
import org.springframework.beans.factory.support.AbstractBeanDefinition

/**
 * Created: 17-03-2014
 * Author: ghik
 */
object AttrNames {
  final val AbstractAttr = "%abstract"
  final val ArgTypesAttr = "%arg-types"
  final val ArrayAttr = "%array"
  final val AutowireAttr = "%autowire"
  final val AutowireCandidateAttr = "%autowire-candidate"
  final val ClassAttr = "%class"
  final val ConstructAttr = "%construct"
  final val ConstructorArgsAttr = "%constructor-args"
  final val DependencyCheckAttr = "%dependency-check"
  final val DependsOnAttr = "%depends-on"
  final val DescriptionAttr = "%description"
  final val DestroyMethodAttr = "%destroy-method"
  final val EntriesAttr = "%entries"
  final val FactoryBeanAttr = "%factory-bean"
  final val FactoryMethodAttr = "%factory-method"
  final val IdrefAttr = "%idref"
  final val IndexAttr = "%index"
  final val InitMethodAttr = "%init-method"
  final val KeyAttr = "%key"
  final val KeyTypeAttr = "%key-type"
  final val LazyInitAttr = "%lazy-init"
  final val ListAttr = "%list"
  final val LookupMethodsAttr = "%lookup-methods"
  final val MergeAttr = "%merge"
  final val MetaAttr = "%meta"
  final val NameAttr = "%name"
  final val ParentAttr = "%parent"
  final val PrimaryAttr = "%primary"
  final val PropsAttr = "%props"
  final val QualifiersAttr = "%qualifiers"
  final val RefAttr = "%ref"
  final val ReplacedMethodsAttr = "%replaced-methods"
  final val ReplacerAttr = "%replacer"
  final val ScopeAttr = "%scope"
  final val SetAttr = "%set"
  final val TypeAttr = "%type"
  final val ValueAttr = "%value"
  final val ValueTypeAttr = "%value-type"
  final val ConfigAttr = "%config"

  final val BeanAttrs = Set(
    AbstractAttr,
    AutowireAttr,
    AutowireCandidateAttr,
    ClassAttr,
    ConstructAttr,
    ConstructorArgsAttr,
    DependencyCheckAttr,
    DependsOnAttr,
    DescriptionAttr,
    DestroyMethodAttr,
    FactoryBeanAttr,
    FactoryMethodAttr,
    InitMethodAttr,
    LazyInitAttr,
    LookupMethodsAttr,
    MetaAttr,
    NameAttr,
    ParentAttr,
    PrimaryAttr,
    QualifiersAttr,
    ReplacedMethodsAttr,
    ScopeAttr)

  final val AutowireMapping = Map(
    "no" -> AbstractBeanDefinition.AUTOWIRE_NO,
    "byName" -> AbstractBeanDefinition.AUTOWIRE_BY_NAME,
    "byType" -> AbstractBeanDefinition.AUTOWIRE_BY_TYPE,
    "constructor" -> AbstractBeanDefinition.AUTOWIRE_CONSTRUCTOR,
    "autodetect" -> AbstractBeanDefinition.AUTOWIRE_AUTODETECT: @silent
  )

  final val ReverseAutowireMapping = AutowireMapping.map(_.swap)

  final val DependencyCheckMapping = Map(
    "none" -> AbstractBeanDefinition.DEPENDENCY_CHECK_NONE,
    "simple" -> AbstractBeanDefinition.DEPENDENCY_CHECK_SIMPLE,
    "objects" -> AbstractBeanDefinition.DEPENDENCY_CHECK_OBJECTS,
    "all" -> AbstractBeanDefinition.DEPENDENCY_CHECK_ALL
  )

  final val ReverseDependencyCheckMapping = DependencyCheckMapping.map(_.swap)
}
