package clojure.lang;

/*-[
#import "NSCommon.h"
]-*/

public class Selector extends RestFn implements Named {

  public final String sel;

  public Selector(Symbol sel) {
    this.sel = sel.name;
  }
  
  public Selector(String sel) {
    this.sel = sel;
  }

  @Override
  public String getNamespace() {
    return null;
  }

  @Override
  public String getName() {
    return sel;
  }
  
  @Override
  protected Object doInvoke(Object o, Object args) {
    if (!ObjC.objc) {
      return RemoteRepl.callRemote(this, RT.cons(o, args));
    } else {
      String sel = this.sel;
      if (args != null && !sel.endsWith(":")) {
        sel = sel + ":";
      }
      return invokeSel(o, sel, RT.seq(args));
    }
  }
  
  public native Object invokeSel(Object object, String selector,
      ISeq arguments) /*-[
   return [NSCommon invokeSel:object withSelector:selector withArgs:arguments];
  ]-*/;
  
  @Override
  public int getRequiredArity() {
    return 1;
  }
}
