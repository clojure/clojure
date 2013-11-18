package clojure.lang;

public class Executors {

  public static java.util.concurrent.ExecutorService newFixedThreadPool(int i,
      java.util.concurrent.ThreadFactory createThreadFactory) {
    return java.util.concurrent.Executors.newFixedThreadPool(i, createThreadFactory);
  }

  public static java.util.concurrent.ExecutorService newCachedThreadPool(
      java.util.concurrent.ThreadFactory createThreadFactory) {
    return java.util.concurrent.Executors.newCachedThreadPool(createThreadFactory);
  }

}
