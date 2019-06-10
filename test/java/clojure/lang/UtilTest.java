package clojure.lang;

import com.sun.org.apache.xpath.internal.operations.String;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import java.math.BigInteger;
import java.util.HashMap;

public class UtilTest {

  @Rule public final ExpectedException thrown = ExpectedException.none();

  @Test
  public void testClassOf() {
    Assert.assertNull(Util.classOf(null));
    Assert.assertEquals(Util.classOf(0), Integer.class);
  }

  @Test
  public void testCompare() {
    Assert.assertEquals(1, Util.compare(false, null));
    Assert.assertEquals(0, Util.compare(1, 1));
    Assert.assertEquals(-1, Util.compare(null, 0));
    Assert.assertEquals(0,
            Util.compare(new Integer(1), new Integer(1)));
  }

  @Test
  public void testEquals() {
    Assert.assertFalse(Util.equals(1, 0));
    Assert.assertFalse(Util.equals(null, 0));

    Assert.assertTrue(Util.equals(1, 1));
  }

  @Test
  public void testEquivWithLong() {
    Assert.assertFalse(Util.equiv(0L, (Object) 1));
    Assert.assertFalse(Util.equiv(1L, 0L));

    Assert.assertTrue(Util.equiv((Object) 0, 0L));
    Assert.assertTrue(Util.equiv(1L, 1L));
  }

  @Test
  public void testEquivWithDouble() {
    Assert.assertFalse(Util.equiv(0.0, (Object) 1.0));
    Assert.assertFalse(Util.equiv(1.0, 0.0));

    Assert.assertTrue(Util.equiv((Object) 0.0, 0.0));
    Assert.assertTrue(Util.equiv(1.0, 1.0));
  }

  @Test
  public void testEquivWithBoolean() {
    Assert.assertFalse(Util.equiv(false,(Object) 0));
    Assert.assertFalse(Util.equiv((Object)0, false));
    Assert.assertFalse(Util.equiv(true, false));

    Assert.assertTrue(Util.equiv(false, false));
  }

  @Test
  public void testEquivWithChar() {
    Assert.assertFalse(Util.equiv('\u0001', '\u0000'));
    Assert.assertFalse(Util.equiv(null, '\u0000'));
    Assert.assertFalse(Util.equiv('\u0000', null));

    Assert.assertTrue(Util.equiv('\u0000', '\u0000'));
  }

  @Test
  public void testEquivWithObject() {
    Assert.assertFalse(Util.equiv(null, new Integer(1)));

    Assert.assertTrue(Util.equiv(null, null));
    Assert.assertTrue(Util.equiv(new Integer(1), new Integer(1)));
  }

  @Test
  public void testEquivPredColl1() {
    final HashMap k1 = new HashMap();
    k1.put(null, null);
    final HashMap k2 = new HashMap();

    Assert.assertFalse(Util.equivPred(k1).equiv(k1, k2));

    k2.put(null, null);

    Assert.assertTrue(Util.equivPred(k1).equiv(k1, k2));
  }

  @Test
  public void testEquivPredColl2() {
    ArraySeq k1 = new ArraySeq(new String[1], 0);
    ArraySeq k2 = new ArraySeq(new String[1], 0);

    Assert.assertTrue(Util.equivPred(k1).equiv(k1, k2));

    k2 = new ArraySeq(new String[2], 0);

    Assert.assertFalse(Util.equivPred(k1).equiv(k1, k2));
  }


  @Test
  public void testEquivPredNull() {
    Object k1 = null;
    Object k2 = null;

    Assert.assertTrue(Util.equivPred(k1).equiv(k1, k2));

    k2 = "foo";

    Assert.assertFalse(Util.equivPred(k1).equiv(k1, k2));
  }

  @Test
  public void testEquivPredEquals() {
    Object k1 = "foo";
    Object k2 = "foo";

    Assert.assertTrue(Util.equivPred(k1).equiv(k1, k2));

    k2 = "Bar";

    Assert.assertFalse(Util.equivPred(k1).equiv(k1, k2));
  }

  @Test
  public void testEquivPredNumber() {
    Object k1 = 1;
    Object k2 = 1;

    Assert.assertTrue(Util.equivPred(k1).equiv(k1, k2));

    k1 = 0;

    Assert.assertFalse(Util.equivPred(k1).equiv(k1, k2));

    k2 = null;

    Assert.assertFalse(Util.equivPred(k1).equiv(k1, k2));

    k1 = null;
    k2 = 0;

    Assert.assertFalse(Util.equivPred(k1).equiv(k1, k2));
  }

  @Test
  public void testHashCombine() {
    Assert.assertEquals(0, Util.hashCombine(908_697_334, 0));
    Assert.assertEquals(483896201, Util.hasheq(BigInt.fromLong(32L)));
    Assert.assertEquals(0, Util.hasheq(null));
    Assert.assertEquals(875635954, Util.hasheq(64));
  }

  @Test
  public void testHash() {
    Assert.assertEquals(0, Util.hash(null));
    Assert.assertEquals(64, Util.hash(64));
    Assert.assertEquals(-1268909715, Util.hash("fooBar"));
  }

  @Test
  public void testIdentical() {
    Assert.assertFalse(Util.identical(0, 1));
    Assert.assertFalse(Util.identical(new Integer(0), new Integer(0)));

    Assert.assertTrue(Util.identical(0, 0));
  }

  @Test
  public void testIsInteger() {
    Assert.assertFalse(Util.isInteger("fooBar"));

    Assert.assertTrue(Util.isInteger(0));
    Assert.assertTrue(Util.isInteger(32L));
    Assert.assertTrue(Util.isInteger(BigInt.fromLong(32L)));
    Assert.assertTrue(Util.isInteger(new BigInteger("1".getBytes())));
  }

  @Test
  public void testIsPrimitive() {
    Assert.assertFalse(Util.isPrimitive(Integer.class));

    Assert.assertTrue(Util.isPrimitive(int.class));
  }

  @Test
  public void testPcequiv() {
    final PersistentVector k1 = new PersistentVector(0, 0, null, null);
    final PersistentVector k2 = new PersistentVector(0, 0, null, null);

    Assert.assertFalse(Util.pcequiv(k1, null));
    Assert.assertFalse(Util.pcequiv(0, k2));

    Assert.assertTrue(Util.pcequiv(k1, k2));
  }

  @Test
  public void testRet1() {
    Assert.assertEquals(0, Util.ret1(0, 0));
  }

  @Test
  public void testRuntimeException() {
    final RuntimeException runtimeException = Util.runtimeException(
            "NullPointerException",
            new Throwable()
    );

    Assert.assertEquals("NullPointerException",
            runtimeException.getMessage());
    Assert.assertEquals("NullPointerException",
            Util.runtimeException("NullPointerException").getMessage());
  }

  @Test
  public void testSneakyThrowThrowable() {
    thrown.expect(Throwable.class);
    Util.sneakyThrow(new Throwable());
  }

  @Test
  public void testSneakyThrowNullPointerException() {
    thrown.expect(NullPointerException.class);
    Util.sneakyThrow(null);
  }
}
