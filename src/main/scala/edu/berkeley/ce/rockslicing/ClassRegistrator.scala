package edu.berkeley.ce.rockslicing

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.twitter.chill.SingletonSerializer
import org.apache.spark.serializer.KryoRegistrator

/**
  * Kryo serializer registrator for all classes that are to be serialized.
  * Modified from from https://gist.github.com/Gekkio/1197273
  */
class ClassRegistrator extends KryoRegistrator {
  override def registerClasses(kryo: Kryo) {
    kryo.register(None.getClass, new SingletonSerializer[AnyRef](None))
    kryo.register(Nil.getClass, new SingletonSerializer[AnyRef](Nil))
    kryo.register(classOf[Some[_]], new SomeSerializer(kryo))
    kryo.register(classOf[scala.collection.mutable.WrappedArray$ofRef])
    kryo.register(classOf[scala.collection.immutable.$colon$colon[_]])
    kryo.register(classOf[Array[scala.Double]])
    kryo.register(classOf[Joint])
    kryo.register(classOf[Block])
    kryo.register(classOf[Array[Block]])
    kryo.register(classOf[Face])
  }
}

/**
  * Serializes Some types using Kryo. See https://github.com/EsotericSoftware/kryo
  * for detailed documentation on using Kryo.
  */
class SomeSerializer(kryo: Kryo) extends Serializer[Some[_]] {
  override def write(kryo: Kryo, output: Output, `object`: Some[_]): Unit = kryo.writeClassAndObject(output, `object`)
  override def read(kryo: Kryo, input: Input, `type`: Class[Some[_]]): Some[_] = Some(kryo.readClassAndObject(input))
}
