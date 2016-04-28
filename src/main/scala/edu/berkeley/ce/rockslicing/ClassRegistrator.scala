package edu.berkeley.ce.rockslicing

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.twitter.chill.SingletonSerializer
import org.apache.spark.serializer.KryoRegistrator

/**
  * Kryo serializer registrator for all classes that are to be serialized.
  */
class ClassRegistrator extends KryoRegistrator {
  override def registerClasses(kryo: Kryo) {
    kryo.register(None.getClass, new SingletonSerializer[AnyRef](None))
    kryo.register(Nil.getClass, new SingletonSerializer[AnyRef](Nil))
    kryo.register(Some.getClass, new SingletonSerializer[AnyRef](Some))
    kryo.register(classOf[scala.collection.mutable.WrappedArray$ofRef])
    kryo.register(classOf[scala.collection.immutable.$colon$colon[_]])
    kryo.register(classOf[Array[scala.Double]])
    kryo.register(classOf[Joint])
    kryo.register(classOf[Block])
    kryo.register(classOf[Array[Block]])
    kryo.register(classOf[Face])
  }
}