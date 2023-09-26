package picante.spice.adapters;

import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import picante.math.intervals.Interval;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

public class SclkRegistryTest {

  private SclkRegistry registry;
  private final TestSclkConverter ckMapm20000 = new TestSclkConverter();
  private final TestSclkConverter ckMapm20001 = new TestSclkConverter();
  private final TestSclkConverter ckMapm2000 = new TestSclkConverter();
  private final TestSclkConverter ckMapm40000 = new TestSclkConverter();
  private final TestSclkConverter ckMap100000 = new TestSclkConverter();
  private final TestSclkConverter sclkm20 = new TestSclkConverter();
  private final TestSclkConverter sclkm2 = new TestSclkConverter();
  private final TestSclkConverter sclkm20000 = new TestSclkConverter();
  private final TestSclkConverter sclkm3 = new TestSclkConverter();

  @Before
  public void setUp() throws Exception {

    Map<Integer, EncodedSCLKConverter> ckMap = new HashMap<Integer, EncodedSCLKConverter>();
    ckMap.put(-20000, ckMapm20000);
    ckMap.put(-20001, ckMapm20001);
    ckMap.put(-2000, ckMapm2000);
    ckMap.put(-40000, ckMapm40000);
    ckMap.put(100000, ckMap100000);
    ckMap.put(-22222, ckMapm20000);

    Map<Integer, Integer> ckSclkIdMap = new HashMap<Integer, Integer>();
    ckSclkIdMap.put(-25000, -20);
    ckSclkIdMap.put(-27000, -20);
    ckSclkIdMap.put(-50000, -3);
    ckSclkIdMap.put(-22222, -20000);
    ckSclkIdMap.put(-55555, -30000);

    Map<Integer, EncodedSCLKConverter> sclkMap = new HashMap<Integer, EncodedSCLKConverter>();
    sclkMap.put(-20, sclkm20);
    sclkMap.put(-2, sclkm2);
    sclkMap.put(-20000, sclkm20000);
    sclkMap.put(-3, sclkm3);

    registry = new SclkRegistry(sclkMap, ckSclkIdMap, ckMap);
  }

  @Test
  public void testSclkRegistry() throws Exception {

    /*
     * Check that the individual maps supplied to the constructor were placed into the appropriate
     * fields, since they are of the same type.
     */
    EncodedSCLKConverter result = registry.getSclk(-40000);
    assertSame(ckMapm40000, result);

    result = registry.getSclk(-3000);
    assertSame(sclkm3, result);
  }

  @Test
  public void testDefaultSclkRetrieval() throws Exception {

    EncodedSCLKConverter result;

    result = registry.getSclk(-20000);
    assertNotSame(sclkm20, result);

    result = registry.getSclk(-20001);
    assertNotSame(sclkm20, result);

    result = registry.getSclk(-25000);
    assertSame(result, sclkm20);

    result = registry.getSclk(-27000);
    assertSame(result, sclkm20);

    result = registry.getSclk(-50000);
    assertSame(result, sclkm3);

    for (int i = -20002; i < -21000; i--) {
      result = registry.getSclk(i);
      assertSame(sclkm20, result);
    }

    result = registry.getSclk(-2000);
    assertNotSame(sclkm2, result);

    for (int i = -2001; i < -3000; i--) {
      result = registry.getSclk(i);
      assertSame(sclkm2, result);
    }

    for (int i = -20000000; i < -20001000; i--) {
      result = registry.getSclk(i);
      assertSame(sclkm20000, result);
    }

    for (int i = -3000; i < -4000; i--) {
      result = registry.getSclk(i);
      assertSame(sclkm3, result);
    }

  }

  @Test
  public void testCkOverrideRetrieval() throws Exception {

    EncodedSCLKConverter result;

    result = registry.getSclk(-20000);
    assertSame(ckMapm20000, result);

    result = registry.getSclk(-20001);
    assertSame(ckMapm20001, result);

    result = registry.getSclk(-2000);
    assertSame(ckMapm2000, result);

    result = registry.getSclk(-40000);
    assertSame(ckMapm40000, result);

    result = registry.getSclk(100000);
    assertSame(ckMap100000, result);

    result = registry.getSclk(-22222);
    assertSame(ckMapm20000, result);

  }

  @Test(expected = AdapterInstantiationException.class)
  public void testIntegerIntegerMapLookupFailure() throws Exception {
    /*
     * This integer code maps to an integer code that is not present in the SCLK map.
     */
    registry.getSclk(-55555);
  }

  @Test(expected = AdapterInstantiationException.class)
  public void testLookupFailure() throws Exception {
    registry.getSclk(-100000);
  }

  @Test(expected = AdapterInstantiationException.class)
  public void testDefaultLookupFailure() throws Exception {
    registry.getSclk(1000);
  }

}


/**
 * Broken implementation of the {@link EncodedSCLKConverter} interface to allow testing of the
 * {@link SclkRegistry}. Unique values are supplied to the constructor to allow identification of
 * the returned function from the registry.
 */
@SuppressWarnings("unused")
class TestSclkConverter implements EncodedSCLKConverter {

  @Override
  public double convertToEncodedSclk(double tdb) {
    return 0;
  }

  @Override
  public double convertToTDB(double encodedSCLK) {
    return 0;
  }

  @Override
  public Interval getTDBRange(Interval buffer) {
    return buffer.setTo(Interval.ALL_DOUBLES);
  }

  @Override
  public Interval getEncodedSclkRange(Interval buffer) {
    return buffer.setTo(Interval.ALL_DOUBLES);
  }

}
