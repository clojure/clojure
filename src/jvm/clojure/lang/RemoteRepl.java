package clojure.lang;

import java.io.IOException;

public class RemoteRepl {

  public static boolean connected;

  static Var callRemoteSel = RT.var("clojure.remoterepl", "call-remote");
  static Var lister = RT.var("clojure.remoterepl", "listen");
  
  public static Object callRemote(Object o, Object seq) {
    if (RemoteRepl.connected) {
      return callRemoteSel.invoke(o, RT.seq(seq));
    } else {
      //throw new RuntimeException("RemoteRepl not connected");
      return null;
    }
  }

  public static void setConnected(boolean connected) {
    RemoteRepl.connected = connected;
  }
  
  public static void listen() {
    try {
      RT.load("clojure/remoterepl");
      lister.invoke();
    } catch (Exception e) {
      throw Util.sneakyThrow(e);
    }
  }
}
