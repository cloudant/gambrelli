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

import org.specs2.mutable.SpecificationWithJUnit
import akka.util.ByteString
import org.specs2.specification.Scope

class ErlangTermDecoderSpec extends SpecificationWithJUnit {

  "Erlang term decoder" should {

    "decode floats efficiently" in new decoder {
      val bs1 = ByteString(131, 70, 64, 94, 221, 47, 26, 159, 190, 119)
      decoder.decode(bs1) must beEqualTo(123.456)

      val bs2 = ByteString(131, 70, 192, 94, 221, 47, 26, 159, 190, 119)
      decoder.decode(bs2) must beEqualTo(-123.456)
    }

    "decode small integers" in new decoder {
      val bs = ByteString(131, 97, 10)
      decoder.decode(bs) must beEqualTo(10)
    }

    "decode large integers" in new decoder {
      val bs1 = ByteString(131, 98, 7, 91, 205, 21)
      decoder.decode(bs1) must beEqualTo(123456789)
      val bs2 = ByteString(131, 98, 248, 164, 50, 235)
      decoder.decode(bs2) must beEqualTo(-123456789)
    }

    "decode floats inefficiently" in new decoder {
      val bs = ByteString(131, 99, 49, 46, 50, 51, 52, 53, 54, 48, 48,
        48, 48, 48, 48, 48, 48, 48, 48, 48, 51, 48, 55, 48, 101, 43, 48, 50,
        0, 0, 0, 0, 0)
      decoder.decode(bs) must beEqualTo(123.456f)
    }

    "decode atoms" in new decoder {
      val bs = ByteString(131, 100, 0, 3, 98, 97, 114)
      decoder.decode(bs) must beEqualTo('bar)
    }

    "decode ports" in new decoder {
      val bs = ByteString(131, 102, 100, 0, 13, 110, 111, 110, 111,
        100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 1, 0)
      decoder.decode(bs) must beEqualTo(
        Port(Symbol("nonode@nohost"), 1, 0))
    }

    "decode pids" in new decoder {
      val bs = ByteString(131, 103, 100, 0, 13, 110, 111, 110, 111,
        100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 46, 0, 0, 0, 0, 0)
      decoder.decode(bs) must beEqualTo(
        Pid(Symbol("nonode@nohost"), 46, 0, 0))
    }

    "decode export fun" in new decoder {
      val bs = ByteString(131, 113, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98,
        97, 114, 97, 1)
      decoder.decode(bs) must beEqualTo(Fun('foo, 'bar, 1))
    }

    "decode references" in new decoder {
      val bs = ByteString(131, 114, 0, 3, 100, 0, 13, 110, 111, 110,
        111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 1, 0, 0, 0, 65, 0,
        0, 0, 0, 0, 0, 0, 0)
      decoder.decode(bs) must beEqualTo(
        Reference(Symbol("nonode@nohost"), 1, List(65, 0, 0)))
    }

    "decode small tuples" in new decoder {
      val bs = ByteString(131, 104, 3, 97, 1, 97, 2, 97, 3)
      decoder.decode(bs) must beEqualTo((1, 2, 3))
    }

    "throw error for small tuples over 22 elements long" in new decoder {
      val bs = ByteString(131, 104, 23, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
        97, 1, 97, 1, 97, 1, 97, 1,
        97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1, 97, 1,
        97, 1,
        97, 1, 97, 1, 97, 1)
      decoder.decode(bs) must throwA[IllegalArgumentException]
    }

    "decode empty list" in new decoder {
      val bs = ByteString(131, 106)
      decoder.decode(bs) must beEqualTo(Nil)
    }

    "decode 'string'" in new decoder {
      val bs = ByteString(131, 107, 0, 3, 65, 66, 67)
      decoder.decode(bs) must beEqualTo(List(65, 66, 67))
    }

    "decode non-empty proper list" in new decoder {
      val bs = ByteString(131, 108, 0, 0, 0, 2, 98, 0, 0, 1,
        244, 98, 0, 0, 1, 244, 106)
      decoder.decode(bs) must beEqualTo(List(500, 500))
    }

    "decode non-empty improper list" in new decoder {
      val bs = ByteString(131, 108, 0, 0, 0, 2, 98, 0, 0, 1, 244, 98, 0, 0,
        1, 244, 98, 0, 0, 1, 244)
      decoder.decode(bs) must beEqualTo(ImproperList(List(500, 500), 500))
    }

    "decode binaries" in new decoder {
      val bs = ByteString(131, 109, 0, 0, 0, 3, 97, 98, 99)
      decoder.decode(bs) must beEqualTo(ByteString("abc"))
    }

    "decode small big numbers" in new decoder {
      val bs1 = ByteString(131, 110, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 16)
      decoder.decode(bs1) must beEqualTo(
        BigInt("1267650600228229401496703205376"))

      val bs2 = ByteString(131, 110, 13, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 16)
      decoder.decode(bs2) must beEqualTo(
        BigInt("-1267650600228229401496703205376"))

      val bs3 = ByteString(131, 110, 1, 0, 0)
      decoder.decode(bs3) must beEqualTo(BigInt("0"))
    }

    "decode large big numbers" in new decoder {
      val bs1 = ByteString(131, 111, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 16)
      decoder.decode(bs1) must beEqualTo(
        BigInt("1267650600228229401496703205376"))

      val bs2 = ByteString(131, 111, 0, 0, 0, 13, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 16)
      decoder.decode(bs2) must beEqualTo(
        BigInt("-1267650600228229401496703205376"))

      val bs3 = ByteString(131, 111, 0, 0, 0, 1, 0, 0)
      decoder.decode(bs3) must beEqualTo(BigInt("0"))
    }

    "decode map" in new decoder {
      val map = Map('abc -> 'cba)
      val bs = ByteString(131, 116, 0, 0, 0, 1,
        100, 0, 3, 97, 98, 99,
        100, 0, 3, 99, 98, 97)
      decoder.decode(bs) must beEqualTo(map)
    }

    "decode all the things at once" in new decoder {
      val bs = ByteString(131, 104, 8, 100, 0, 3, 102, 111, 111, 107, 0, 3,
        102, 111, 111, 109, 0, 0, 0, 3, 102, 111, 111, 97, 1, 99, 49, 46, 48,
        48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
        48, 101, 43, 48, 48, 0, 0, 0, 0, 0, 106, 103, 100, 0, 13, 110, 111, 110,
        111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 31, 0, 0, 0,
        0, 0, 114, 0, 3, 100, 0, 13, 110, 111, 110, 111, 100, 101, 64, 110, 111,
        104, 111, 115, 116, 0, 0, 0, 0, 37, 0, 0, 0, 0, 0, 0, 0, 0)
      decoder.decode(bs) must beEqualTo(('foo, List(102, 111, 111),
        ByteString("foo"), 1, 1.0, Nil,
        Pid(Symbol("nonode@nohost"), 31, 0, 0),
        Reference(Symbol("nonode@nohost"), 0, List(37, 0, 0))))
    }

  }

}

trait decoder extends Scope {
  val decoder = new ErlangTermDecoder
}
