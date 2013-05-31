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

import org.specs2.specification.Scope
import org.specs2.mutable.SpecificationWithJUnit
import akka.util.ByteString

class ErlangTermEncoderSpec extends SpecificationWithJUnit {

  "Erlang term decoder" should {

    "encode floats efficiently" in new encoder {
      val bs1 = ByteString(131, 70, 64, 94, 221, 47, 26, 159, 190, 119)
      encoder.encode(123.456) must beEqualTo(bs1)

      val bs2 = ByteString(131, 70, 192, 94, 221, 47, 26, 159, 190, 119)
      encoder.encode(-123.456) must beEqualTo(bs2)
    }

    "encode small ints" in new encoder {
      encoder.encode(12 toShort) must beEqualTo(ByteString(131, 97, 12))
    }

    "encode large ints" in new encoder {
      encoder.encode(10000) must beEqualTo(ByteString(131, 98, 0, 0, 39, 16))
      encoder.encode(-10000) must beEqualTo(
        ByteString(131, 98, 255, 255, 216, 240))
    }

    "encode symbols" in new encoder {
      encoder.encode('abc) must beEqualTo(ByteString(131, 100, 0, 3, 97, 98, 99))
    }

    "encode ports" in new encoder {
      val port = Port(Symbol("nonode@nohost"), 1, 0)
      val bs = ByteString(131, 102, 100, 0, 13, 110, 111, 110, 111,
        100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 1, 0)
      encoder.encode(port) must beEqualTo(bs)
    }

    "encode pids" in new encoder {
      val pid = Pid(Symbol("nonode@nohost"), 46, 0, 0)
      val bs = ByteString(131, 103, 100, 0, 13, 110, 111, 110, 111,
        100, 101, 64, 110, 111, 104, 111, 115, 116, 0, 0, 0, 46, 0, 0, 0, 0, 0)
      encoder.encode(pid) must beEqualTo(bs)
    }

    "encode export fun" in new encoder {
      val fun = Fun('foo, 'bar, 1)
      val bs = ByteString(131, 113, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98,
        97, 114, 97, 1)
      encoder.encode(fun) must beEqualTo(bs)
    }

    "encode references" in new encoder {
      val ref = Reference(Symbol("nonode@nohost"), 1, List(65, 0, 0))
      val bs = ByteString(131, 114, 0, 3, 100, 0, 13, 110, 111, 110,
        111, 100, 101, 64, 110, 111, 104, 111, 115, 116, 1, 0, 0, 0, 65, 0,
        0, 0, 0, 0, 0, 0, 0)
      encoder.encode(ref) must beEqualTo(bs)
    }

    "encode small tuples" in new encoder {
      val tuple = (1, 2, 3)
      val bs = ByteString(131, 104, 3, 97, 1, 97, 2, 97, 3)
      encoder.encode(tuple) must beEqualTo(bs)
    }

    "encode empty list" in new encoder {
      val list = Nil
      val bs = ByteString(131, 106)
      encoder.encode(list) must beEqualTo(bs)
    }

    "encode 'string'" in new encoder {
      val list = List(65, 66, 67)
      val bs = ByteString(131, 107, 0, 3, 65, 66, 67)
      encoder.encode(list) must beEqualTo(bs)
    }

    "encode non-empty proper list" in new encoder {
      val list = List(500, 500)
      val bs = ByteString(131, 108, 0, 0, 0, 2, 98, 0, 0, 1,
        244, 98, 0, 0, 1, 244, 106)
      encoder.encode(list) must beEqualTo(bs)
    }

    "encode non-empty improper list" in new encoder {
      val list = ImproperList(List(500, 500), 500)
      val bs = ByteString(131, 108, 0, 0, 0, 2, 98, 0, 0, 1, 244, 98, 0, 0,
        1, 244, 98, 0, 0, 1, 244)
      encoder.encode(list) must beEqualTo(bs)
    }

    "encode binary" in new encoder {
      encoder.encode(ByteString(97, 98, 99)) must beEqualTo(
        ByteString(131, 109, 0, 0, 0, 3, 97, 98, 99))
    }

    "encode large big numbers" in new encoder {
      val big1 = BigInt("1267650600228229401496703205376")
      val bs1 = ByteString(131, 111, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 16)
      encoder.encode(big1) must beEqualTo(bs1)

      val big2 = BigInt("-1267650600228229401496703205376")
      val bs2 = ByteString(131, 111, 0, 0, 0, 13, 1, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 16)
      encoder.encode(big2) must beEqualTo(bs2)

      val big3 = BigInt("0")
      val bs3 = ByteString(131, 111, 0, 0, 0, 1, 0, 0)
      encoder.encode(big3) must beEqualTo(bs3)
    }

    "encode complex thing" in new encoder {
      val thing = (1, 2, 3, ByteString("foo"), 'foo)
      val bs = ByteString(131, 104, 5, 97, 1, 97, 2, 97, 3, 109, 0, 0, 0, 3,
        102, 111, 111, 100, 0, 3, 102, 111, 111)
      encoder.encode(thing) must beEqualTo(bs)
    }

  }

}

trait encoder extends Scope {
  val encoder = new ErlangTermEncoder
}
