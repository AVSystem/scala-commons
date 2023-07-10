package com.avsystem.commons
package spring

import com.avsystem.commons.spring.AttrNames._
import com.typesafe.config._
import org.springframework.beans.factory.annotation.Qualifier
import org.springframework.beans.factory.config.ConstructorArgumentValues.ValueHolder
import org.springframework.beans.factory.config.{BeanDefinitionHolder, ConstructorArgumentValues, RuntimeBeanNameReference, RuntimeBeanReference}
import org.springframework.beans.factory.support._
import org.springframework.beans.{MutablePropertyValues, PropertyValue}
import org.springframework.core.io.Resource

import java.{util => ju}
import scala.annotation.nowarn

class HoconBeanDefinitionReader(registry: BeanDefinitionRegistry)
  extends AbstractBeanDefinitionReader(registry) {

  import com.avsystem.commons.spring.HoconBeanDefinitionReader.Keys._
  import com.typesafe.config.ConfigValueType._

  private implicit class ConfigValueExtensions(value: ConfigValue) {
    def as[T: HoconType]: T =
      implicitly[HoconType[T]].get(value)
  }

  private val autowireMapping = AutowireMapping.withDefault {
    v => throw new IllegalArgumentException(s"Invalid value $v for $AutowireAttr attribute")
  }

  private val dependencyCheckMapping = DependencyCheckMapping.withDefault {
    v => throw new IllegalArgumentException(s"Invalid value $v for $DependencyCheckAttr attribute")
  }

  private def setup[T](t: T)(setupFunc: T => Any) = {
    setupFunc(t)
    t
  }

  private def iterate(obj: ConfigObject)
    (attrFun: (String, ConfigValue) => Any)
    (propFun: (String, ConfigValue) => Any) =
    obj.asScala.foreach {
      case (key, _) if key.startsWith("_") =>
      case (key, value) if key.startsWith("%") => attrFun(key, value)
      case (key, value) => propFun(key, value)
    }

  private def validateObj(
    required: Set[String] = Set.empty,
    requiredAny: Set[String] = Set.empty,
    allowed: Set[String] = Set.empty,
    props: Boolean = false
  )(obj: ConfigObject): Unit = {
    require(required.forall(obj.containsKey),
      s"Attributes ${required.mkString(", ")} must be present in object at ${obj.origin.description}")
    require(requiredAny.isEmpty || requiredAny.exists(obj.containsKey),
      s"At least one of ${requiredAny.mkString(", ")} must be present in object at ${obj.origin.description}")
    val allAllowed = required ++ requiredAny ++ allowed
    iterate(obj) { (key, value) =>
      if (!allAllowed.contains(key))
        badAttr(key, value)
    } { (key, value) =>
      if (!props)
        badProp(key, value)
    }
  }

  @nowarn("msg=deprecated")
  private def getProps(obj: ConfigObject) =
    obj.asScala.filterKeys(k => !k.startsWith("%") && !k.startsWith("_"))

  private def badAttr(key: String, value: ConfigValue) =
    throw new IllegalArgumentException(s"Unexpected attribute $key at ${value.origin.description}")

  private def badProp(key: String, value: ConfigValue) =
    throw new IllegalArgumentException(s"Unexpected property $key at ${value.origin.description}")

  private object BeanDefinition {
    val BeanOnlyAttrs = BeanAttrs - MetaAttr

    def unapply(obj: ConfigObject) =
      if (BeanOnlyAttrs.exists(obj.as[ConfigObject].keySet.contains)) Some(obj) else None
  }

  private class ObjectWithAttributePresentExtractor(elementAttr: String) {
    def unapply(obj: ConfigObject) =
      if (obj.containsKey(elementAttr)) Some(obj) else None
  }

  private object ListDefinition extends ObjectWithAttributePresentExtractor(ListAttr)

  private object ArrayDefinition extends ObjectWithAttributePresentExtractor(ArrayAttr)

  private object SetDefinition extends ObjectWithAttributePresentExtractor(SetAttr)

  private object PropertiesDefinition extends ObjectWithAttributePresentExtractor(PropsAttr)

  private object ValueDefinition extends ObjectWithAttributePresentExtractor(ValueAttr)

  private object BeanReference extends ObjectWithAttributePresentExtractor(RefAttr)

  private object BeanNameReference extends ObjectWithAttributePresentExtractor(IdrefAttr)

  private object RawConfig extends ObjectWithAttributePresentExtractor(ConfigAttr)

  private def read(value: ConfigValue): Any = value match {
    case BeanDefinition(obj) =>
      val bd = readBean(obj)
      obj.get(NameAttr).as[Option[String]] match {
        case Some(name) => new BeanDefinitionHolder(bd, name)
        case None => bd
      }
    case BeanReference(obj) => readRef(obj)
    case BeanNameReference(obj) => readIdref(obj)
    case ArrayDefinition(obj) => readArray(obj)
    case ListDefinition(obj) => readList(obj)
    case SetDefinition(obj) => readSet(obj)
    case PropertiesDefinition(obj) => readProperties(obj)
    case RawConfig(obj) => readRawConfig(obj)
    case obj: ConfigObject => readMap(obj)
    case list: ConfigList => readRawList(list)
    case _ => value.unwrapped
  }

  private def readRef(obj: ConfigObject) = {
    validateObj(required = Set(RefAttr), allowed = Set(ParentAttr))(obj)
    new RuntimeBeanReference(obj.get(RefAttr).as[String], obj.get(ParentAttr).as[Option[Boolean]].getOrElse(false))
  }

  private def readIdref(obj: ConfigObject) = {
    validateObj(required = Set(IdrefAttr))(obj)
    new RuntimeBeanNameReference(obj.get(IdrefAttr).as[String])
  }

  private def readList(obj: ConfigObject) = {
    validateObj(required = Set(ListAttr), allowed = Set(MergeAttr, ValueTypeAttr))(obj)
    setup(new ManagedList[Any]) { list =>
      list.addAll(obj.get(ListAttr).as[ConfigList].asScala.map(read).asJavaCollection)
      list.setMergeEnabled(obj.get(MergeAttr).as[Option[Boolean]].getOrElse(false))
      list.setElementTypeName(obj.get(ValueTypeAttr).as[Option[String]].orNull)
    }
  }

  private def readArray(obj: ConfigObject) = {
    validateObj(required = Set(ArrayAttr), allowed = Set(MergeAttr, ValueTypeAttr))(obj)
    val elements = obj.get(ArrayAttr).as[ConfigList]
    val valueType = obj.get(ValueTypeAttr).as[Option[String]].getOrElse("")
    val result = new ManagedArray(valueType, elements.size)
    result.addAll(elements.asScala.map(v => read(v).asInstanceOf[AnyRef]).asJavaCollection)
    result.setMergeEnabled(obj.get(MergeAttr).as[Option[Boolean]].getOrElse(false))
    result
  }

  private def readSet(obj: ConfigObject) = {
    validateObj(required = Set(SetAttr), allowed = Set(MergeAttr, ValueTypeAttr))(obj)
    setup(new ManagedSet[Any]) { set =>
      set.addAll(obj.get(SetAttr).as[ConfigList].asScala.map(read).asJavaCollection)
      set.setMergeEnabled(obj.get(MergeAttr).as[Option[Boolean]].getOrElse(false))
      set.setElementTypeName(obj.get(ValueTypeAttr).as[Option[String]].orNull)
    }
  }

  private def readRawList(list: ConfigList) = {
    setup(new ManagedList[Any]) { ml =>
      ml.addAll(list.asScala.map(read).asJavaCollection)
    }
  }

  private def readMap(obj: ConfigObject) = {
    validateObj(allowed = Set(MergeAttr, KeyTypeAttr, ValueTypeAttr, EntriesAttr), props = true)(obj)
    setup(new ManagedMap[Any, Any]) { mm =>
      mm.setMergeEnabled(obj.get(MergeAttr).as[Option[Boolean]].getOrElse(false))
      mm.setKeyTypeName(obj.get(KeyTypeAttr).as[Option[String]].orNull)
      mm.setValueTypeName(obj.get(ValueTypeAttr).as[Option[String]].orNull)
      obj.get(EntriesAttr).as[Option[ConfigList]].getOrElse(ju.Collections.emptyList).asScala.foreach {
        case obj: ConfigObject =>
          validateObj(required = Set(KeyAttr, ValueAttr))(obj)
          mm.put(read(obj.get(KeyAttr)), read(obj.get(ValueAttr)))
        case _ =>
          throw new IllegalArgumentException(s"Required an object at ${obj.origin.description}")
      }
      getProps(obj).foreach {
        case (key, value) => mm.put(key, read(value))
      }
    }
  }

  private def readProperties(obj: ConfigObject) = {
    validateObj(required = Set(PropsAttr), allowed = Set(MergeAttr))(obj)
    setup(new ManagedProperties) { mp =>
      mp.setMergeEnabled(obj.get(MergeAttr).as[Option[Boolean]].getOrElse(false))
      obj.get(PropsAttr).as[Option[Config]].getOrElse(ConfigFactory.empty).entrySet.asScala.foreach {
        case entry if Set(STRING, NUMBER, BOOLEAN).contains(entry.getValue.valueType) =>
          mp.setProperty(entry.getKey, entry.getValue.unwrapped.toString)
        case entry => throw new IllegalArgumentException(s"Bad prop definition at ${entry.getValue.origin.description}")
      }
    }
  }

  private def readRawConfig(obj: ConfigObject) = {
    validateObj(required = Set(ConfigAttr))(obj)
    obj.get(ConfigAttr).as[Config]
  }

  private def readBean(obj: ConfigObject) = {
    val bd = new GenericBeanDefinition
    val cargs = new ConstructorArgumentValues
    val propertyValues = new MutablePropertyValues
    bd.setConstructorArgumentValues(cargs)
    bd.setPropertyValues(propertyValues)
    bd.setResourceDescription(obj.origin.description)

    def addConstructorArg(idxAndValue: (Option[Int], ValueHolder)) = idxAndValue match {
      case (Some(idx), valueHolder) => cargs.addIndexedArgumentValue(idx, valueHolder)
      case (None, valueHolder) => cargs.addGenericArgumentValue(valueHolder)
    }

    obj.get(AbstractAttr).as[Option[Boolean]].foreach(bd.setAbstract)
    obj.get(AutowireCandidateAttr).as[Option[Boolean]].foreach(bd.setAutowireCandidate)
    obj.get(AutowireAttr).as[Option[String]].map(autowireMapping).foreach(bd.setAutowireMode)
    obj.get(ClassAttr).as[Option[String]].foreach(bd.setBeanClassName)
    readConstructorArgs(obj.get(ConstructorArgsAttr)).foreach(addConstructorArg)
    obj.get(DependencyCheckAttr).as[Option[String]].map(dependencyCheckMapping).foreach(bd.setDependencyCheck)
    obj.get(DescriptionAttr).as[Option[String]].foreach(bd.setDescription)
    obj.get(DestroyMethodAttr).as[Option[String]].foreach(bd.setDestroyMethodName)
    obj.get(DependsOnAttr).as[Option[ju.List[String]]].map(_.asScala.toArray).foreach(bd.setDependsOn(_: _*))
    obj.get(FactoryBeanAttr).as[Option[String]].foreach(bd.setFactoryBeanName)
    obj.get(FactoryMethodAttr).as[Option[String]].foreach(bd.setFactoryMethodName)
    obj.get(InitMethodAttr).as[Option[String]].foreach(bd.setInitMethodName)
    bd.setLazyInit(obj.get(LazyInitAttr).as[Option[Boolean]].getOrElse(false))
    obj.get(LookupMethodsAttr).as[Option[ConfigObject]].foreach { obj =>
      validateObj(props = true)(obj)
      getProps(obj).foreach {
        case (key, value) => bd.getMethodOverrides.addOverride(new LookupOverride(key, value.as[String]))
      }
    }
    obj.get(MetaAttr).as[Option[ConfigObject]].getOrElse(ConfigFactory.empty.root).asScala.foreach {
      case (mkey, mvalue) => bd.setAttribute(mkey, mvalue.as[String])
    }
    obj.get(ParentAttr).as[Option[String]].foreach(bd.setParentName)
    obj.get(PrimaryAttr).as[Option[Boolean]].foreach(bd.setPrimary)
    obj.get(QualifiersAttr).as[Option[ju.List[ConfigObject]]].getOrElse(ju.Collections.emptyList).asScala.foreach { obj =>
      bd.addQualifier(readQualifier(obj))
    }
    obj.get(ReplacedMethodsAttr).as[Option[ju.List[ConfigObject]]].getOrElse(ju.Collections.emptyList).asScala.foreach { obj =>
      bd.getMethodOverrides.addOverride(readReplacedMethod(obj))
    }
    obj.get(ScopeAttr).as[Option[String]].foreach(bd.setScope)

    val construct = obj.get(ConstructAttr).as[Option[Boolean]].getOrElse(false)
    getProps(obj).foreach {
      case (key, value) =>
        if (construct) {
          addConstructorArg(readConstructorArg(value, forcedName = key))
        } else {
          propertyValues.addPropertyValue(readPropertyValue(key, value))
        }
    }
    bd
  }

  private def readQualifier(obj: ConfigObject) = {
    validateObj(allowed = Set(TypeAttr, ValueAttr), props = true)(obj)
    val acq = new AutowireCandidateQualifier(obj.get(TypeAttr).as[Option[String]].getOrElse(classOf[Qualifier].getName))
    obj.get(ValueAttr).as[Option[String]].foreach(acq.setAttribute(AutowireCandidateQualifier.VALUE_KEY, _))
    getProps(obj).foreach {
      case (key, value) => acq.setAttribute(key, value.as[String])
    }
    acq
  }

  private def readReplacedMethod(obj: ConfigObject) = {
    validateObj(required = Set(NameAttr, ReplacerAttr), allowed = Set(ArgTypesAttr))(obj)
    val replaceOverride = new ReplaceOverride(obj.get(NameAttr).as[String], obj.get(ReplacerAttr).as[String])
    obj.get(ArgTypesAttr).as[Option[ju.List[String]]].getOrElse(ju.Collections.emptyList).asScala.foreach(replaceOverride.addTypeIdentifier)
    replaceOverride
  }

  private def readConstructorArgs(value: ConfigValue) = {
    value.as[Option[Either[ConfigList, ConfigObject]]] match {
      case Some(Left(list)) =>
        list.iterator.asScala.zipWithIndex.map { case (configValue, idx) =>
          readConstructorArg(configValue, forcedIndex = idx)
        }
      case Some(Right(obj)) =>
        validateObj(props = true)(obj)
        getProps(obj).iterator.map { case (name, configValue) =>
          readConstructorArg(configValue, forcedName = name)
        }
      case None =>
        Iterator.empty
    }
  }

  private def readConstructorArg(
    value: ConfigValue,
    forcedIndex: OptArg[Int] = OptArg.Empty,
    forcedName: OptArg[String] = OptArg.Empty
  ) = value match {
    case ValueDefinition(obj) =>
      validateObj(required = Set(ValueAttr), allowed = Set(IndexAttr, TypeAttr, NameAttr))(obj)
      val vh = new ValueHolder(read(obj.get(ValueAttr)))
      obj.get(TypeAttr).as[Option[String]].foreach(vh.setType)
      (forcedName.toOption orElse obj.get(NameAttr).as[Option[String]]).foreach(vh.setName)
      val indexOpt = forcedIndex.toOption orElse obj.get(IndexAttr).as[Option[Int]]
      (indexOpt, vh)
    case _ =>
      val vh = new ValueHolder(read(value))
      forcedName.foreach(vh.setName)
      (forcedIndex.toOption, vh)
  }

  private def readPropertyValue(name: String, value: ConfigValue) = value match {
    case ValueDefinition(obj) =>
      validateObj(required = Set(ValueAttr), allowed = Set(MetaAttr))(obj)
      val pv = new PropertyValue(name, read(obj.get(ValueAttr)))
      obj.get(MetaAttr).as[Option[ConfigObject]].getOrElse(ConfigFactory.empty.root).asScala.foreach {
        case (mkey, mvalue) => pv.setAttribute(mkey, mvalue.as[String])
      }
      pv
    case _ =>
      new PropertyValue(name, read(value))
  }

  private def readBeans(obj: ConfigObject) = {
    validateObj(props = true)(obj)
    val beanDefs = getProps(obj).iterator.flatMap {
      case (key, value) =>
        try {
          value.as[Option[ConfigObject]].map(obj => (key, readBean(obj)))
        } catch {
          case e: Exception => throw new RuntimeException(
            s"Could not read definition of bean $key at ${value.origin.description}", e)
        }
    }.toVector
    beanDefs.foreach((registry.registerBeanDefinition _).tupled)
    beanDefs.size
  }

  private def readAliases(obj: ConfigObject): Unit = {
    validateObj(props = true)(obj)
    getProps(obj).foreach {
      case (key, value) => value.as[Option[String]].foreach(registry.registerAlias(_, key))
    }
  }

  private def readConditionals(config: Config): Config = {
    if (!config.hasPath(Conditionals)) config
    else config.getList(Conditionals).asScala.foldLeft(config.withoutPath(Conditionals)) { (currentConfig, conditionalObject) =>
      val props = getProps(conditionalObject.as[ConfigObject])

      if (props(Condition).as[Boolean])
        readConditionals(props(Config).as[Config]).withFallback(currentConfig)
      else
        currentConfig
    }
  }

  def loadBeanDefinitions(resourceConfig: Config): Int = {
    val config = readConditionals(resourceConfig)
    val beans = if (config.hasPath(Beans)) config.getObject(Beans) else ConfigFactory.empty.root
    val aliases = if (config.hasPath(Aliases)) config.getObject(Aliases) else ConfigFactory.empty.root
    val result = readBeans(beans)
    readAliases(aliases)
    result
  }

  def loadBeanDefinitions(resource: Resource): Int =
    loadBeanDefinitions(ConfigFactory.parseURL(resource.getURL).resolve)
}
object HoconBeanDefinitionReader {
  object Keys {
    final val Conditionals = "conditionals"
    final val Condition = "condition"
    final val Config = "config"
    final val Beans = "beans"
    final val Aliases = "aliases"
  }
}
