package u06lab.code

import java.util
import java.util.Collection
/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combine[A : Combiner](a : List[A]) : A


object FunctionsImpl extends Functions:
  override def combine[A : Combiner](coll : List[A]): A =
    if coll.isEmpty then summon[Combiner[A]].unit
    else coll.foldRight(summon[Combiner[A]].unit)(summon[Combiner[A]].combine(_, _))
  override def sum(a: List[Double]): Double =
    if a.isEmpty then 0
    else a.sum
  override def concat(a: Seq[String]): String =
    if a.isEmpty then ""
    else a.toList reduce(_ + _)

  override def max(a: List[Int]): Int =
    if a.isEmpty then Integer.MIN_VALUE
    else a.max[Int]


/*
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 *   to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 *   the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A
//  def combine(b: List[A]): A

object bho:
  given Combiner[Double] with
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b
  given Combiner[Int] with
    override def unit: Int = Integer.MIN_VALUE
    override def combine(a: Int, b: Int): Int = if a > b then a else b
  given Combiner[String] with
    override def unit: String = ""
    override def combine(a: String, b: String): String = a + b

import bho.given_Combiner_Double
import bho.given_Combiner_Int
import bho.given_Combiner_String

@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
//  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
//  println(f.sum(List())) // 0.0
//  println(f.concat(Seq("a", "b", "c"))) // abc
//  println(f.concat(Seq())) // ""
//  println(f.max(List(-10, 3, -5, 0))) // 3
//  println(f.max(List())) // -2147483648
  println(f.combine(List[Int]())) // 0.0
  println(f.combine(List(10.0, 20.0, 30.1)))
  println(f.combine(List[Double]())) // -2147483648
  println(f.combine(Seq("a", "b", "c").toList)) // abc
  println(f.combine(List[String]())) // ""
  println(f.combine(List(-10, 3, -5, 0))) // 3
//  println(f.combine(List()))
