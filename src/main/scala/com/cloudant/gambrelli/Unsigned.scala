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

import akka.util.{ByteStringBuilder, ByteIterator}
import java.nio.ByteOrder

object Unsigned {

  implicit val byteOrder = ByteOrder.BIG_ENDIAN

  def unsignedByte(it: ByteIterator): Short = {
    (it.getByte & 0xFF).toShort
  }

  def unsignedShort(it: ByteIterator): Int = {
    it.getShort & 0xFFFF
  }

  def unsignedInt(it: ByteIterator): Long = {
    it.getInt & 0x00000000FFFFFFFFL
  }

  def putUnsignedByte(b: ByteStringBuilder, v: Short) {
    b.putByte(v toByte)
  }

  def putUnsignedShort(b: ByteStringBuilder, v: Int) {
    b.putShort(v toShort)
  }

  def putUnsignedInt(b: ByteStringBuilder, v: Long) {
    b.putInt(v toInt)
  }

}
