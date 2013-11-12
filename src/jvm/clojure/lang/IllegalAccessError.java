package clojure.lang;

import java.lang.RuntimeException;
import java.lang.String;

public class IllegalAccessError extends RuntimeException {

  public IllegalAccessError(String msg) {
    super(msg);
  }
}
