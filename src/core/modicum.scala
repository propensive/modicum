/*
    Modicum, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package modicum

import rudiments.*
import contingency.*
import fulminate.*
import hypotenuse.*

import scala.collection.mutable as scm

import language.experimental.captureChecking

case class MemoryError(amount: ByteSize)
extends Error(m"a request was made for $amount more memory than has been allocated")

case class Allowance(amount: ByteSize, retain: ByteSize):
  var free: ByteSize = amount
  var retained: ByteSize = 0.b
  val blocks: scm.Map[ByteSize, List[Array[Byte]]] = scm.HashMap().withDefault { _ => Nil }

  def allocate(requirement: ByteSize): Array[Byte] raises MemoryError = synchronized:
    if requirement < free then blocks(requirement) match
      case head :: tail =>
        blocks(requirement) = tail
        free -= requirement
        head

      case Nil =>
        free -= requirement
        new Array[Byte](requirement.long.toInt)
    else abort(MemoryError(requirement - free))

  def deallocate(array: Array[Byte]): Unit =
    synchronized:
      val size = array.size.b
      free += size

      if retained < retain then
        blocks(size) ::= array
        retained += size

def limit(amount: ByteSize)[ResultType](block: Allowance ?=> ResultType): ResultType =
  val allowance = Allowance(amount, (amount.long/2).b)
  block(using allowance)

def allocate
    [ResultType]
    (amount: ByteSize)
    (using allowance: Allowance, excessMemory: Errant[MemoryError])
    (block: Array[Byte]^ => ResultType)
    : ResultType =

  val array = allowance.allocate(amount)
  block(array).also(allowance.deallocate(array))
