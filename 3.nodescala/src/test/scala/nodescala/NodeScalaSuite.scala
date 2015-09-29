package nodescala

import nodescala.NodeScala._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.collection.{immutable, mutable}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.language.postfixOps
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("All Futures should succeed") {
    val list = List(1, 2)
    val successes = list map (n => Future.always(n))
    val all = Future.all(successes)
    assert(Await.result(all, 1 second) == list)
  }

  test("At least one Future should fail") {
    val list = List(Future(1), Future(throw new RuntimeException), Future(2))
    val all = Future.all(list)
    try {
      Await.result(all, 1 second)
      assert(false)
    } catch {
      case e: TimeoutException => assert(false)
      case NonFatal(e) =>
    }
  }

  test("At least one Future should succeed") {
    val list = List(
      Future { Thread.sleep(200); throw new RuntimeException },
      Future { Thread.sleep(200); throw new RuntimeException },
      Future { Thread.sleep(200); throw new RuntimeException },
      Future { Thread.sleep(100); 2 },
      Future { Thread.sleep(200); throw new RuntimeException })
    val any = Future.any(list)
    assert(Await.result(any, 1 second) == 2)
  }

  test("Future should complete with RuntimeException") {
    val Ex = new RuntimeException
    val list = List(
      Future { Thread.sleep(200); 10 },
      Future { Thread.sleep(200); 10 },
      Future { Thread.sleep(200); 10 },
      Future { Thread.sleep(100); throw Ex })
    val any = Future.any(list)
    try {
      Await.result(any, 1 second)
      assert(false)
    } catch {
      case Ex =>
      case NonFatal(e) => assert(false)
    }
  }

  test("Delay should not complete before its timeout") {
    val duration = 100 millis
    val f = Future.delay(duration)
    try {
      Await.result(f, duration - (10 millis))
      assert(false)
    } catch {
      case e: TimeoutException =>
      case NonFatal(e) => assert(false)
    }
  }

  test("Should be complete by its timeout") {
    val duration = 100 millis
    val f = Future.delay(duration)
    try {
      Await.result(f, duration + (10 millis))
    } catch {
      case NonFatal(e) => assert(false)
    }
  }

  test("A successful Future should return its value") {
    assert(Future.always(10).now == 10)
  }

  test("A failed Future should throw its exception") {
    val Ex = new RuntimeException
    try {
      Future.failed(Ex).now
      assert(false)
    } catch {
      case Ex =>
      case NonFatal(e) => assert(false)
    }
  }

  test("An uncompleted Future should throw a NoSuchElementException") {
    try {
      Future.never.now
      assert(false)
    } catch {
      case e: NoSuchElementException =>
      case NonFatal(e) => assert(false)
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




