package clojure.lang;

public class SourceWriter {

  private final StringBuilder sb = new StringBuilder();
  private int tab = 0;
  
  public void tab() {
    tab++;
  }
  
  public void untab() {
    tab --;
  }
  
  public void println(String l) {
    if (tab > 0) {
      sb.append(new String(new char[tab]).replace("\0", " "));
    }
    sb.append(l).append("\n");
  }
  
  @Override
  public String toString() {
    return sb.toString();
  }

  public void println() {
    sb.append("\n");
  }
}
