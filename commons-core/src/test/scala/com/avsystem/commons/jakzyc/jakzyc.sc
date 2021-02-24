import scala.util.{Failure, Success, Try}

// SYNCHRONOUS

trait DeviceProtocolApi {
  // blocking API to communicate with the device
  def getParam(name: String): Int
  def setParam(name: String, value: Int): Unit
}

def businessLogic(diwajs: DeviceProtocolApi): Int = {
  val paramName = "counter"
  val value = diwajs.getParam(paramName)
  Thread.sleep(1000)
  val newValue = value + 1
  diwajs.setParam(paramName, newValue)
  newValue
}

// NAIVE CALLBACKS

trait CallbackDeviceProtocolApi {
  // blocking API to communicate with the device
  def getParam(name: String)(callback: Int => Unit): Unit
  def setParam(name: String, value: Int)(callback: Unit => Unit): Unit
}

object Sleeper {
  def sleep(millis: Long)(callback: Unit => Unit): Unit = ???
}

def callbackBusinessLogic(diwajs: CallbackDeviceProtocolApi)(callback: Int => Unit): Unit = {
  val paramName = "counter"
  diwajs.getParam(paramName) { value =>
    Sleeper.sleep(1000) { _ =>
      val newValue = value + 1
      diwajs.setParam(paramName, newValue) { _ =>
        callback(newValue)
      }
    }
  }
}

// CURRIED, FAILURE AWARE CALLBACKS

type Callback[-T] = Try[T] => Unit
type Async[+T] = Callback[T] => Unit

trait AsyncDeviceProtocolApi {
  // blocking API to communicate with the device
  def getParam(name: String): Async[Int]
  def setParam(name: String, value: Int): Async[Unit]
}

object AsyncSleeper {
  def sleep(millis: Long): Async[Unit] = ???
}

def asyncBusinessLogic(diwajs: AsyncDeviceProtocolApi): Async[Int] = { callback =>
  val paramName = "counter"
  diwajs.getParam(paramName) {
    case Failure(cause) => callback(Failure(cause))
    case Success(value) =>
      AsyncSleeper.sleep(1000) {
        case Failure(cause) => callback(Failure(cause))
        case Success(_) =>
          val newValue = value + 1
          diwajs.setParam(paramName, newValue) {
            case Failure(cause) => callback(Failure(cause))
            case Success(_) => callback(Success(newValue))
          }
      }
  }
}

// PROPER ASYNC TYPE WITH MAP/FLATMAP ETC

case class AsyncTask[+T](run: Callback[T] => Unit) {
  def map[S](f: T => S): AsyncTask[S] =
    AsyncTask { callbackForS: Callback[S] =>
      val callbackForT: Callback[T] =
        (tryT: Try[T]) => callbackForS(tryT.map(f))
      run(callbackForT)
    }

  def flatMap[S](f: T => AsyncTask[S]): AsyncTask[S] =
    AsyncTask { callbackForS: Callback[S] =>
      val callbackForT: Callback[T] = {
        case Success(t: T) => Try(f(t)) match {
          case Success(taskS: AsyncTask[S]) => taskS.run(callbackForS)
          case Failure(cause) => callbackForS(Failure(cause))
        }
        case Failure(cause) => callbackForS(Failure(cause))
      }
      run(callbackForT)
    }
}
object AsyncTask {
  def pure[T](value: T): AsyncTask[T] =
    AsyncTask(callback => callback(Success(value)))

  def eval[T](expr: => T): AsyncTask[T] =
    AsyncTask(callback => callback(Try(expr)))

  def failed[T](cause: Throwable): AsyncTask[T] =
    AsyncTask(callback => callback(Failure(cause)))
}

trait AsyncTaskDeviceProtocolApi {
  // blocking API to communicate with the device
  def getParam(name: String): AsyncTask[Int]
  def setParam(name: String, value: Int): AsyncTask[Unit]
}

object AsyncTaskSleeper {
  def sleep(millis: Long): AsyncTask[Unit] = ???
}

def flatMapAsyncTaskBusinessLogic(diwajs: AsyncTaskDeviceProtocolApi): AsyncTask[Int] = {
  val paramName = "counter"
  diwajs.getParam(paramName).flatMap { value =>
    AsyncTaskSleeper.sleep(1000).flatMap { _ =>
      val newValue = value + 1
      diwajs.setParam(paramName, newValue).map { _ =>
        newValue
      }
    }
  }
}

def forComprehensionAsyncTaskBusinessLogic(diwajs: AsyncTaskDeviceProtocolApi): AsyncTask[Int] =
  for {
    paramName <- AsyncTask.pure("counter")
    value <- diwajs.getParam(paramName)
    _ <- AsyncTaskSleeper.sleep(1000)
    newValue = value + 1
    _ <- diwajs.setParam(paramName, newValue)
  } yield newValue
