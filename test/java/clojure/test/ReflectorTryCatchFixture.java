package clojure.test;

public class ReflectorTryCatchFixture {

	public static void fail(Long x) throws Cookies {
		throw new Cookies("Long");
	}
	
	public static void fail(Double y) throws Cookies {
		throw new Cookies("Double");
	}
	
	public static class Cookies extends Exception {
		public Cookies(String msg) { super(msg); }
	}

}
