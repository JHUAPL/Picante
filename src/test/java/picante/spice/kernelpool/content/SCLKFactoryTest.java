package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernel.tk.lsk.UniformTimeProvider;
import picante.spice.kernel.tk.sclk.SCLKInstantiationException;
import picante.spice.kernel.tk.sclk.SCLKKernel;
import picante.spice.kernelpool.BasicKernelPool;

public class SCLKFactoryTest {

  List<Double> doubles;
  BasicKernelPool pool;
  BasicKernelPool emptyPool;
  BasicKernelPool unsupportedTypePool;
  BasicKernelPool instantiationFailurePool;
  SCLKFactory factory;

  @Before
  public void setUp() throws Exception {

    doubles = new ArrayList<Double>();
    pool = new BasicKernelPool();
    emptyPool = new BasicKernelPool();
    unsupportedTypePool = new BasicKernelPool();
    instantiationFailurePool = new BasicKernelPool();

    factory = new SCLKFactory(new UniformTimeProvider() {

      @Override
      public double convertToTDB(double parallelTime) {
        return parallelTime;
      }

      @Override
      public double convertToUniformTime(double tdb) {
        return tdb;
      }
    });

    doubles.add(0.0);
    doubles.add(0.0);
    doubles.add(1.0);
    doubles.add(10000.0);
    doubles.add(10000.0);
    doubles.add(1.0);
    pool.addDoubles("SCLK01_COEFFICIENTS_82", doubles);
    pool.addDoubles("SCLK01_COEFFICIENTS_123", doubles);
    unsupportedTypePool.addDoubles("SCLK01_COEFFICIENTS_82", doubles);
    doubles.clear();
    doubles.add(0.0);
    pool.addDoubles("SCLK_PARTITION_START_82", doubles);
    pool.addDoubles("SCLK_PARTITION_START_123", doubles);
    unsupportedTypePool.addDoubles("SCLK_PARTITION_START_82", doubles);
    instantiationFailurePool.addDoubles("SCLK_PARTITION_START_82", doubles);
    doubles.clear();
    doubles.add(1000000.0);
    pool.addDoubles("SCLK_PARTITION_END_82", doubles);
    pool.addDoubles("SCLK_PARTITION_END_123", doubles);
    unsupportedTypePool.addDoubles("SCLK_PARTITION_END_82", doubles);
    instantiationFailurePool.addDoubles("SCLK_PARTITION_END_82", doubles);
    doubles.clear();
    doubles.add(1000000.0);
    pool.addDoubles("SCLK01_MODULI_82", doubles);
    pool.addDoubles("SCLK01_MODULI_123", doubles);
    unsupportedTypePool.addDoubles("SCLK01_MODULI_82", doubles);
    instantiationFailurePool.addDoubles("SCLK01_MODULI_82", doubles);
    doubles.clear();
    doubles.add(1.0);
    pool.addDoubles("SCLK_DATA_TYPE_82", doubles);
    pool.addDoubles("SCLK_DATA_TYPE_123", doubles);
    unsupportedTypePool.addDoubles("SCLK_DATA_TYPE_82", doubles);
    instantiationFailurePool.addDoubles("SCLK_DATA_TYPE_82", doubles);
    doubles.clear();
    doubles.add(2.0);
    unsupportedTypePool.addDoubles("SCLK_DATA_TYPE_123", doubles);

    pool.addDoubles("SCLK01_OFFSETS_82", Arrays.asList(0.0));
    pool.addDoubles("SCLK01_OFFSETS_123", Arrays.asList(1.0));

  }

  @Test(expected = SCLKInstantiationException.class)
  public void testCreateConvertersInstantiationException() throws Exception {
    factory.createConverters(instantiationFailurePool);
  }

  @Test(expected = SCLKInstantiationException.class)
  public void testCreateConvertersUnsupportedTypeException() throws Exception {
    factory.createConverters(unsupportedTypePool);
  }

  @Test
  public void testCreateConverters() throws Exception {
    Map<Integer, SCLKKernel> result = factory.createConverters(pool);
    assertTrue(result.containsKey(-82));
    assertTrue(result.containsKey(-123));
    assertEquals(0.0, result.get(-82).convertToEncodedSclk(0.0), 0.0);
    assertEquals(10000.0, result.get(-123).convertToTDB(10000.0), 0.0);
  }

}
