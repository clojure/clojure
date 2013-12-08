package clojure.lang;

public class Executors {

  public static ExecutorService newFixedThreadPool(int i) {
    //newFixedThreadPool 
    return new ExecutorService(i);
  }

  public static ExecutorService newCachedThreadPool() {
    //newCachedThreadPool
    return new ExecutorService();
  }

}
