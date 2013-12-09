package clojure.lang;

public class RestFnWithMeta extends RestFn {

  final AFunction aFunction;
  final IPersistentMap meta;

  public RestFnWithMeta(AFunction aFunction, IPersistentMap meta) {
    this.aFunction = aFunction;
    this.meta = meta;
  }

  protected Object doInvoke(Object args) {
    return aFunction.applyTo((ISeq) args);
  }

  public IPersistentMap meta() {
    return meta;
  }

  public IObj withMeta(IPersistentMap meta) {
    return aFunction.withMeta(meta);
  }

  public int getRequiredArity() {
    return 0;
  }
}
