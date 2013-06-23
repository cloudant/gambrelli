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

case class Reference(node: Symbol, creation: Short, id: Seq[Long])

case class Port(node: Symbol, id: Long, creation: Short)

case class Pid(node: Symbol, id: Long, serial: Long, creation: Short)

case class ImproperList(list: Seq[Any], tail: Any)

case class Fun(module: Symbol, function: Symbol, arity: Short)

class ArbitraryTuple[A](elements: Seq[A]) extends Product {

  def productArity: Int = elements.size

  def productElement(n: Int) = elements(n)

  def canEqual(that: Any) = that match {
    case _: Product =>
      true
    case _ =>
      false
  }

  override def equals(other: Any): Boolean = other match {
    case other: Product =>
      if (productArity != other.productArity) {
        return false
      }
      for (j <- 0 to productArity - 1) {
        if (productElement(j) != other.productElement(j)) {
          return false
        }
      }
      true
    case _ =>
      false
  }

  override def toString = {
    elements.toString()
  }

}
