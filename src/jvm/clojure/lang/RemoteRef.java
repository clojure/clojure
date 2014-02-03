package clojure.lang;

public class RemoteRef extends RestFn {

  private static final String OBJC_REF = "objc-ref-";

  private static final String JVM_REF = "jvm-ref-";

  private static Atom a = new Atom(RT.map());

  private static Var gensym = RT.var("clojure.core", "gensym");

  public static Object register(final Object fn) {
    final String id = ((Symbol) ((AFn) gensym.getRawRoot()).invoke(ObjC.objc ? OBJC_REF : JVM_REF)).getName();
    a.swap(new AFn() {
      @Override
      public Object invoke(Object old) {
        return RT.assoc(old, id, fn);
      }
    });
    return id;
  }

  private String id;

  public RemoteRef(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public Object get() {
    if (ObjC.objc) {
      if (id.startsWith(JVM_REF)) {
        return this;
      } else {
        return (Object) RT.get(a.deref(), id);  
      }
    } else {
      if (id.startsWith(OBJC_REF)) {
        return this;
      } else {
        return (Object) RT.get(a.deref(), id);  
      }
    }
  }
  
  @Override
  protected Object doInvoke(Object args) {
    return RemoteRepl.callRemote(this, args);
  }

  @Override
  public int getRequiredArity() {
    return 0;
  }
}
