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

import akka.util.ByteStringBuilder

trait TypeEncoder {
  def unapply(any: Any): Boolean

  def encode(any: Any, it: ByteStringBuilder)
}

object NoneTypeEncoder extends TypeEncoder {
  def unapply(any: Any) = false

  def encode(any: Any, it: ByteStringBuilder) {}
}

object BinaryAsStringEncoder extends TypeEncoder {
  import Unsigned._

  def unapply(any: Any) = any match {
    case _: String => true
    case _ => false
  }

  def encode(any: Any, b: ByteStringBuilder) {
    any match {
      case str: String =>
        val bytes = str.getBytes("UTF-8")
        putUnsignedByte(b, 109)
        putUnsignedInt(b, bytes length)
        b ++= bytes
    }
  }
}
