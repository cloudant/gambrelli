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

import akka.util.ByteIterator
import com.cloudant.gambrelli.Unsigned._

trait TypeDecoder {
  def unapply(ord: Short): Boolean

  def decode(ord: Short, it: ByteIterator): Any
}


object NoneTypeDecoder extends TypeDecoder {
  def unapply(ord: Short) = false

  def decode(ord: Short, it: ByteIterator): Any = ()
}

object BinaryAsStringDecoder extends TypeDecoder {
  def unapply(ord: Short) = ord == 109

  def decode(ord: Short, it: ByteIterator) = ord match {
    case 109 =>
      val len = unsignedInt(it).toInt
      val bytes = new Array[Byte](len)
      it.getBytes(bytes)
      new String(bytes, "UTF-8")
  }
}
