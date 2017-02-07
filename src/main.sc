/**
  * Outline of function definition
  * trait Function1[-A, +R] {
  * def apply(x: A): R
  * }
  *
  * Benefit of functions being trait is that we can
  * subclass functions in scala
  */

/**
  * {case "ping" => "pong"}
  * Above statement will give error that argument type of
  * anonymous function must be fully known. So type should
  * be specified externally like this
  * var f: String => String = {case "ping" => "pong"}
  */

/**
  * Partial Functions
  * If we want to check if a function is defined for a
  * specific input then we can use partial functions
  * instead of a normal function, because partial function
  * has isDefinedAt() along with apply method
  */

val f: PartialFunction[String, String] = {
  case "Ping" => "Pong"
}

f("Ping")
f.isDefinedAt("abc")

/**
  * isDefinedAt only checks for outer most pattern match
  * so if we have nested pattern matches that isDefinedAt
  * will not make sure that MatchError is not thrown
  */

/**
  * For expressions
  */

def isEven(sum: Int) = {
  sum % 2 == 0
}
for {
  i <- 1 until 10
  j <- 1 until 20
  if isEven(i + j)
} yield (i, j)

/**
  * TODO: learn this from internet
  * We can also have pattern matching in for loop
  *
  * val data: List[JSON] = ...
  * for {
  * JObj(binding) <- data
  * JSeq(phones) = binding("phoneNumbers")
  * JObj(phone) <- phones
  * JStr(digits) = phone("number")
  * if digits startsWith "212"
  * } yield (binding("firstName"))
  */

/**
  * For expressions are similar to database queries
  * We can use for expressions with not just
  * collections but anything that implements map,
  * flatmap and filter, so if a database connection
  * implements flatmap, map and filter then we can use for expressions
  * with database
  */

/**
  * ScalaCheck is a testing tool to generate random values
  * for testing so it is used along with ScalaTest
  */

/**
  * Monads
  * A type with a flat map is monad if it follows 3 monads laws
  * 1 - Associativity
  * m flatmap f flatmap g == m flatmap (x => f(x)  flatmap g)
  * 2 - Left unit law
  * unit(x) flatmap f == f(x)
  * 3 - Right unit law
  * m flatmap unit == m
  *
  * Monads can normally be used with for but for is not only limited
  * to monads because even if left unit law fails for can be used
  * because left unit law does not hold any special meaning in context
  * of for
  */

/**
  * Streams
  * Streams are same as List but they are lazy
  */

1 #:: 2 #:: Stream.empty
(1 to 10000) toStream

/**
  * Lazy val evaluation
  * def also defines an expression to be evaluated when called and
  * not at time of definition like functions, but difference between
  * lazy and def is that def is computed every time it is called
  * while lazy val is evaluated only once.
  */

lazy val a = {
  println("1 + 2")
  1 + 2
}

a + a

/**
  * Infinite Streams
  */

def infiniteInts(initialDigit: Int): Stream[Int] = initialDigit #:: infiniteInts(initialDigit + 1)

infiniteInts(1) take 10 toList

/**
  * Seive method for finding prime numbers can be easily implemented
  * using infinite streams. In this method take first prime no 2
  * and then eliminate all multiples of it then you take next prime
  * no and eliminate all multiple of it
  */

def seive(s: Stream[Int]): Stream[Int] =
s.head #:: seive(s.tail filter (_ % s.head != 0))

val primes = seive(infiniteInts(2))

primes take 100 toList

/**
  * Water pouring problem
  * Basically given a list of glasses if we are said to fill the glasses to given capacity
  * and we can only either empty a glass, fill it completely or pour it to other glass
  */

/**
  *
  * @param capacity State of glasses to be filled
  */
class Pouring(capacity: Vector[Int]) {

  // State is amount of water in all glasses
  type State = Vector[Int]

  val initialState = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State) = state updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State) = state updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  val glasses = capacity.indices

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for (from <- glasses; to <- glasses; if from != to) yield Pour(from, to))

  // Paths
  class Path(history: List[Move], val endState: State) {
    // fold right takes default value and a function which when given a move and state
    // applies move change to a state.

    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString: String = (history.reverse mkString " ") + "-->" + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, more map (_.endState))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}

val pouring = new Pouring(Vector(4, 9))
pouring.solutions(6)

/**
  * Name everything you can
  * Put operations in natural scope
  * Keep degree of freedom for future changes
  */

/**
  * Function and state
  */

class BankAccount {
  private var balance = 0

  def deposit(amount: Int) = {
    if (amount > 0) balance += amount
  }

  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance) {
      balance -= amount
      balance
    } else throw new Error("Insufficient Balance")
}

val account = new BankAccount
account deposit 50
account withdraw 30
account withdraw 10
account withdraw 20

/**
  * BankAccount is a stateful object because
  * result of withdraw depends on history, its
  * not purely functional
  */

val x1 = new BankAccount
val y1 = new BankAccount  // x1 and y1 are different objects
val x2 = new BankAccount
val y2 = x2               // x2 and y2 are same objects

/**
  * We can not use substitution model because we can not replace
  * x2 with new BankAccount as it will make x2 and y2 different
  * objects. So we can not use substitution model with non purely
  * functional code
  */

/**
  * Observer pattern
  * In observer pattern there is a publisher and many
  * observers. Each observer has a notify method and publisher
  * has subscribe, unsubscribe methods and list of observables.
  */

/**
  * Functional reactive programming
  * Instead of event based programming where on every event we
  * update list of observers, we can use an abstract over and define
  * Signals. Signals are time to value mappings, which describe change of
  * value over time. Forexample instead of mouseMoved event we can have
  * mousePosition Signal which will give use mouse position at any given time.
  * Signal() // calling apply on signal will give its value at current time.
  * We can define one signal in terms of others:
  * val isUpperArea : Signal[Boolean] = Signal {
  *   val pos = mousePosition()
  *   pos.x > 20 && pos.y > 20
  * }
  *
  * So now at every point of time this boolean signal will tell if
  * mouse is in upper position or not
  *
  * We can also have signal which change over time, they are called Var.
  * Var have update method which allows to change signal definition over time.
  * val sig = Var(3)
  * sig.update(5)
  * or we can write above statement as sig() = 5
  * Make sure you are not defining a signal in terms of itself if you want to do
  * it store signal value in a variable and then use that variable to define.
  *
  * Example:
  * val num = Signal(1)
  * val twice = Signal(num() * 2)
  */

/**
  * #Monads
  * Monad is a way to build by joining simple computations. A monad computation
  * can be chained together with other monad computations to define
  * functionality in elegant way. A monad must follow four monad laws
  */


/**
  * Four Essential effects in Programming
  * - Try, a computation can fail
  * - Future, a computation can take time and fail
  * - Iterable, a computation can return many results
  * - Observable, a computation can return many results over time
  */

/**
  * #Try
  * When we want to express in type that a computation can fail we make
  * computation return Try[T].
  */

/**
  * #Future
  * When we want to express in type that computation will take time and fail.
  * Future trait has an onComplete method:
  * - which either take two methods success (T=> Unit) and failed (Throwable => Unit)
  * - Or it take Observable, which has two methods onNext and onError
  *
  * In addition to standard monad functions like map, flatMap, filter ... It has
  * an additional method recoverWith which is called on failure
  * As Future has flatMap so we can use for expressions with Futures
  */

  /**
  * # Async Await
  * We can use async await to handle future values
  * val future: Future[S] = async {
  *   val x: T = await { http.get etc }
  *   val y: S = await { http.post(x) }
  *   y
  * }
  * Basically async block returns a future, inside async block you can use
  * await to wait for other futures to return and once you are done you return a
  * value which is then wrapped in a future and return from async block
  */

/**
  * #Promises
  * Basically promise and future are complimentary concepts. You get a f
  */
