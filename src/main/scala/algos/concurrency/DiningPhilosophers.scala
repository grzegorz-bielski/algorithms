package algos.concurrency.philisophers

import java.util.concurrent.Semaphore
import scala.util.Random

// 5 philosophers sit at a round table. Each has a plate of spaghetti. A fork is placed between each pair of adjacent
// philosophers. A philosopher needs both forks to eat. Each philosopher can pick up the fork on their left or the fork
// on their right, but cannot start eating until they have both forks. Forks need to be pick up one at the time. When a philosopher finishes eating, they put down
// both forks and start thinking. The goal is to design a concurrent program that allows the philosophers to eat without
// starving.

@main
def run =
  val n = 5
  val forks = Vector.tabulate(n)(_ => Fork())
  val philosophers = Vector.tabulate(n)(Philosopher(_, forks))

  philosophers.foreach(Thread(_).start()) // fire and forget

final class Fork:
  private val lock = Semaphore(1)

  def pickUp(): Unit = lock.acquire()
  def putDown(): Unit = lock.release()

final class Philosopher(index: Int, _forks: Vector[Fork]) extends Runnable:
  import Philosopher.*

  private var state = State.Thinking

  // deadlocks are avoided by always picking up the left fork first
  private val (leftFork, rightFork) =
    val a = index
    val b = (index + 1) % _forks.size
    val (left, right) = if a < b then (b, a) else (a, b)

    (_forks(left), _forks(right))

  def run(): Unit =
    while true do
      state match
        case State.Hungry =>
          leftFork.pickUp()
          rightFork.pickUp()
          println(s"Philosopher $index is eating")
          state = State.Eating // we have both forks, time to eat

        case State.Eating =>
          Thread.sleep(Random.between(2000, 7000)) // om nom nom
          println(s"Philosopher $index is thinking")
          state = State.Thinking // time to think

        case State.Thinking =>
          rightFork.putDown()
          leftFork.putDown()
          Thread.sleep(Random.between(1000, 5000)) // hmm...
          println(s"Philosopher $index is hungry")
          state = State.Hungry // hungry again ...

object Philosopher:
  enum State:
    case Thinking, Hungry, Eating
