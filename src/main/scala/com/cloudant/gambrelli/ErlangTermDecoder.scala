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

import akka.util._
import java.nio.ByteOrder

class ErlangTermDecoder {

  implicit val byteOrder = ByteOrder.BIG_ENDIAN

  def decode(bs: ByteString): Any = {
    val it = bs.iterator
    if (unsignedByte(it) != 131) {
      throw new IllegalArgumentException("Not an erlang term")
    }
    decode(it)
  }

  def decode(it: ByteIterator): Any = {
    unsignedByte(it) match {
      case 70 => // NEW_FLOAT_EXT
        it.getDouble
      case 97 => // SMALL_INTEGER_EXT
        unsignedByte(it)
      case 98 => // INTEGER_EXT
        it.getInt
      case 99 => // FLOAT_EXT
        dropSlice(it, 0, 31).toByteString.decodeString("latin1").toFloat
      case 100 => // ATOM_EXT
        val len = unsignedShort(it)
        Symbol(dropSlice(it, 0, len).toByteString.decodeString("latin1"))
      case 102 => // PORT_EXT
        val node = decode(it).asInstanceOf[Symbol]
        Port(node, unsignedInt(it), unsignedByte(it))
      case 103 => // PID_EXT
        val node = decode(it).asInstanceOf[Symbol]
        Pid(node, unsignedInt(it), unsignedInt(it), unsignedByte(it))
      case 104 => // SMALL_TUPLE_EXT
        val arity = unsignedByte(it)
        makeTuple(it, arity)
      case 106 => // NIL_EXT
        Nil
      case 107 => // STRING_EXT
        val len = unsignedShort(it)
        dropSlice(it, 0, len).toList
      case 108 => // LIST_EXT
        val len = unsignedInt(it).toInt
        val elements = for (i <- 1 to len) yield {
          decode(it)
        }
        decode(it) match {
          case Nil =>
            elements
          case tail =>
            ImproperList(elements, tail)
        }
      case 109 => // BINARY_EXT
        val len = unsignedInt(it).toInt
        dropSlice(it, 0, len).toByteString
      case 110 => // SMALL_BIG_EXT
        val len = unsignedByte(it)
        val sign = if (unsignedByte(it) == 1) -1 else 1
        val magnitude = new Array[Byte](len)
        it.getBytes(magnitude)
        BigInt(sign, magnitude reverse)
      case 111 => // LARGE_BIG_EXT
        val len = unsignedInt(it).toInt
        val sign = if (unsignedByte(it) == 1) -1 else 1
        val magnitude = new Array[Byte](len)
        it.getBytes(magnitude)
        BigInt(sign, magnitude reverse)
      case 113 => // EXPORT_EXT
        Fun(decode(it).asInstanceOf[Symbol],
        decode(it).asInstanceOf[Symbol],
        decode(it).asInstanceOf[Short])
      case 114 => // NEW_REFERENCE_EXT
        val len = unsignedShort(it)
        val node = decode(it).asInstanceOf[Symbol]
        val creation = unsignedByte(it)
        val id = for (i <- 1 to len) yield {
          unsignedInt(it)
        }
        Reference(node, creation, id)
      case 116 => // MAP_EXT
        val size = unsignedInt(it)
        val keys = for (i <- 1L to size) yield decode(it)
        val values = for (i <- 1L to size) yield decode(it)
        keys.zip(values).toMap
    }
  }

  private def unsignedByte(it: ByteIterator): Short = {
    (it.getByte & 0xFF).toShort
  }

  private def unsignedShort(it: ByteIterator): Int = {
    it.getShort & 0xFFFF
  }

  private def unsignedInt(it: ByteIterator): Long = {
    it.getInt & 0x00000000FFFFFFFFL
  }

  private def dropSlice(it: ByteIterator,
                        from: Int,
                        until: Int): ByteIterator = {
    val slice = it.clone().slice(from, until)
    it.drop(until - from)
    slice
  }

  private def makeTuple(it: ByteIterator, arity: Short): Product = {
    arity match {
      case 1 => Tuple1(decode(it))
      case 2 => Tuple2(decode(it), decode(it))
      case 3 => Tuple3(decode(it), decode(it), decode(it))
      case 4 => Tuple4(decode(it), decode(it), decode(it), decode(it))
      case 5 => Tuple5(decode(it), decode(it), decode(it), decode(it),
        decode(it))
      case 6 => Tuple6(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it))
      case 7 => Tuple7(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it))
      case 8 => Tuple8(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it))
      case 9 => Tuple9(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it))
      case 10 => Tuple10(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it), decode(it))
      case 11 => Tuple11(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it))
      case 12 => Tuple12(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it))
      case 13 => Tuple13(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it))
      case 14 => Tuple14(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it))
      case 15 => Tuple15(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it), decode(it))
      case 16 => Tuple16(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it))
      case 17 => Tuple17(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it))
      case 18 => Tuple18(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it))
      case 19 => Tuple19(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it))
      case 20 => Tuple20(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it), decode(it))
      case 21 => Tuple21(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it))
      case 22 => Tuple22(decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it), decode(it),
        decode(it), decode(it), decode(it), decode(it), decode(it), decode(it))
      case _ =>
        throw new IllegalArgumentException("Tuples of " + arity +
          " not supported")
    }
  }

}
