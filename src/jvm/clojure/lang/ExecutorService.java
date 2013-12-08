package clojure.lang;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class ExecutorService implements Executor {

  public ExecutorService() {
  }

  public ExecutorService(int i) {
  }

  public void shutdown() {
  }

  public Future submit(final Callable callable) {
    return new Future() {
      Object r = null;
      
      boolean done = false;
      boolean cancelled = false;
      
      Thread t = new Thread() {
        public void run() {
          try {
            r = callable.call();
            done = true;
          } catch (Exception e) {
            throw Util.sneakyThrow(e);
          }
        }
      };

      @Override
      public boolean cancel(boolean mayInterruptIfRunning) {
        cancelled = true;
        // TODO ?
        return true;
      }

      @Override
      public Object get() throws InterruptedException, ExecutionException {
        return r;
      }

      @Override
      public Object get(long timeout, TimeUnit unit)
          throws InterruptedException, ExecutionException, TimeoutException {
        long millis = unit.toMillis(timeout);
        long start = System.currentTimeMillis();
        while (true) {
          Thread.sleep(50);
          if (done) { 
            return r;
          } else if (System.currentTimeMillis() - start > millis) {
            throw new TimeoutException();
          }
        }
      }

      @Override
      public boolean isCancelled() {
        return cancelled;
      }

      @Override
      public boolean isDone() {
        return done;
      }
    };
  }

  @Override
  public void execute(Runnable arg0) {
    new Thread(arg0).start();
  }
}
