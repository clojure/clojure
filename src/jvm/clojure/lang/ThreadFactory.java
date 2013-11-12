package clojure.lang;

public interface ThreadFactory {
  Thread newThread(java.lang.Runnable runnable);
}
