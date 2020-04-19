package edu.berkeley.ce.sparkrocks

import com.esotericsoftware.kryo.Kryo
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
    // kryo.register(classOf[scala.collection.immutable.Stream$Cons]);
    // kryo.register(classOf[scala.collection.Iterator$$anonfun$toStream$1])
    // kryo.register(classOf[scala.collection.MapLike$$anon$2])
    // kryo.register(classOf[scala.collection.immutable.HashMap$HashTrieMap$$anon$1])
    // kryo.register(classOf[Array[scala.collection.immutable.HashMap]])
    kryo.register(classOf[Array[scala.Double]])
    kryo.register(classOf[Joint])
    kryo.register(classOf[Block])
    kryo.register(classOf[Array[Block]])
    kryo.register(classOf[Block])    
    kryo.register(classOf[Face])
    kryo.register(classOf[BlockDem])
    kryo.register(classOf[Array[BlockDem]])
    kryo.register(classOf[BlockVTK])
    kryo.register(classOf[Array[BlockVTK]])        
  }
}
