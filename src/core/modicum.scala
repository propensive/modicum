package modicum

import rudiments.*
import perforate.*
import fulminate.*
import hypotenuse.*

import scala.collection.mutable as scm

import language.experimental.captureChecking

case class MemoryError(amount: ByteSize)
extends Error(msg"a request was made for $amount more memory than has been allocated")

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
    (using allowance: Allowance, excessMemory: Raises[MemoryError])
    (block: Array[Byte]^ => ResultType)
    : ResultType =
  
  val array = allowance.allocate(amount)
  block(array).also(allowance.deallocate(array))
  
