package modicum

import rudiments.*

import perforate.*
import errorHandlers.throwUnsafely

import language.experimental.captureChecking

@main
def run(): Unit =
  limit(5.mb):
    println("1")

    var x: Array[Byte] = Array()

    println:
      allocate(1.mb): array =>
        println(array)
        array

    allocate(4.mb): array =>
      println(array)

    allocate(1.mb): array =>
      println(array)