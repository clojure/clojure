package clojure.test;

public class ReflectorTryCatchFixture {

	public static void fail(Long x) throws Cookies {
		throw new Cookies("Long");
	}
	
	public static void fail(Double y) throws Cookies {
		throw new Cookies("Double");
	}
	
        public void failWithCause(Double y) throws Cookies {
                throw new Cookies("Wrapped", new Cookies("Cause"));
	}
	
	public static class Cookies extends Exception {
                public Cookies(String msg, Throwable cause) { super(msg, cause); }
		public Cookies(String msg) { super(msg); }
	}

}
