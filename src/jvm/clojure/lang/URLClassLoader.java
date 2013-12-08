package clojure.lang;

import java.net.URL;

public class URLClassLoader extends ClassLoader  {
  
  public URLClassLoader(URL[] emptyUrls, ClassLoader classLoader) {
    super(classLoader);
  }

  public void addURL(URL url) {
    throw new RuntimeException(url.toString());
  }
}
