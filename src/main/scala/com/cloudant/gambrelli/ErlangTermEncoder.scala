/*
 * Copyright 2013 Cloudant.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package com.cloudant.gambrelli

import java.nio.ByteOrder
import akka.util.{ByteStringBuilder, ByteString}

class ErlangTermEncoder {

  implicit val byteOrder = ByteOrder.BIG_ENDIAN

  def encode(any: Any): ByteString = {
    val b = new ByteStringBuilder
    putUnsignedByte(b, 131)
    encode(b, any)
    b.result()
  }

  def encode(b: ByteStringBuilder, any: Any) {
    any match {
      case f: Float =>
        encode(b, f toDouble)
      case d: Double =>
        putUnsignedByte(b, 70)
        b.putDouble(d)
      case s: Short =>
        putUnsignedByte(b, 97)
        putUnsignedByte(b, s)
      case i: Int =>
        if (i > 0 && i < 256) {
          encode(b, i toShort)
        } else {
          putUnsignedByte(b, 98)
          b.putInt(i)
        }
      case s: Symbol =>
        putUnsignedByte(b, 100)
        putUnsignedShort(b, s.name size)
        b.putBytes(s.name.getBytes("latin1"))
      case p: Port =>
        putUnsignedByte(b, 102)
        encode(b, p.node)
        putUnsignedInt(b, p.id)
        putUnsignedByte(b, p.creation)
      case p: Pid =>
        putUnsignedByte(b, 103)
        encode(b, p.node)
        putUnsignedInt(b, p.id)
        putUnsignedInt(b, p.serial)
        putUnsignedByte(b, p.creation)
      case Nil =>
        putUnsignedByte(b, 106)
      case l: List[_] =>
        if (isString(l)) {
          putUnsignedByte(b, 107)
          putUnsignedShort(b, l length)
          for (item <- l) {
            putUnsignedByte(b, item.asInstanceOf[Int] toShort)
          }
        } else {
          encodeList(b, l)
          encode(b, Nil)
        }
      case l: ImproperList =>
        encodeList(b, l.list)
        encode(b, l.tail)
      case bs: ByteString =>
        putUnsignedByte(b, 109)
        putUnsignedInt(b, bs size)
        b ++= bs
      case bi: BigInt =>
        val bytes = bi.abs.toByteArray
        putUnsignedByte(b, 111)
        putUnsignedInt(b, bytes length)
        putUnsignedByte(b, if (bi.signum == -1) 1 else 0)
        b.putBytes(bytes reverse)
      case f: Fun =>
        putUnsignedByte(b, 113)
        encode(b, f.module)
        encode(b, f.function)
        encode(b, f.arity)
      case r: Reference =>
        putUnsignedByte(b, 114)
        putUnsignedShort(b, r.id.length)
        encode(b, r.node)
        putUnsignedByte(b, r.creation)
        for (v <- r.id) {
          putUnsignedInt(b, v)
        }
      case t: Product => // must be last as case classes are Products too.
        putUnsignedByte(b, 104)
        putUnsignedByte(b, t.productArity toShort)
        for (item <- t.productIterator) {
          encode(b, item)
        }
    }
  }

  private def isString(l: List[Any]) = {
    l.length < 65536 && l.forall {x =>
      x match {
        case x:Int =>
          x >= 0 && x < 128
        case _ => false
      }
    }
  }

  private def encodeList(b: ByteStringBuilder, l: Seq[Any]) {
    putUnsignedByte(b, 108)
    putUnsignedInt(b, l length)
    for (item <- l) {
      encode(b, item)
    }
  }

  private def putUnsignedByte(b: ByteStringBuilder, v: Short) {
    b.putByte(v toByte)
  }

  private def putUnsignedShort(b: ByteStringBuilder, v: Int) {
    b.putShort(v toShort)
  }

  private def putUnsignedInt(b: ByteStringBuilder, v: Long) {
    b.putInt(v toInt)
  }

}