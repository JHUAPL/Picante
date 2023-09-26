package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import picante.spice.kernel.tk.lsk.LSK;
import picante.spice.kernel.tk.lsk.LSKInstantiationException;
import picante.spice.kernelpool.KernelPool;

public class LSKFactoryTest {

  private LSKFactory factory;
  private KernelPool pool;
  private KernelPool badPool;

  // TODO: Add tests for leapseconds table

  @Before
  public void setUp() throws Exception {
    pool = new KernelPool();
    badPool = new KernelPool();

    List<Double> doubles = new ArrayList<Double>();
    doubles.add(1.657E-3);
    pool.addDoubles("DELTET/K", doubles);
    badPool.addDoubles("DELTET/K", doubles);

    doubles.set(0, 1.671E-2);
    pool.addDoubles("DELTET/EB", doubles);
    badPool.addDoubles("DELTET/EB", doubles);

    doubles.set(0, 6.239996E0);
    doubles.add(1.99096871E-7);
    pool.addDoubles("DELTET/M", doubles);

    doubles.clear();
    doubles.add(32.184);
    pool.addDoubles("DELTET/DELTA_T_A", doubles);
    badPool.addDoubles("DELTET/DELTA_T_A", doubles);

    doubles.clear();
    doubles.add(10.0);
    doubles.add(-8.83656E8);
    doubles.add(11.0);
    doubles.add(-8.679312E8);
    pool.addDoubles("DELTET/DELTA_AT", doubles);

    factory = new LSKFactory();
  }

  @Test
  public void testCreateLSK() throws Exception {
    LSK lsk = factory.createLSK(pool);
    assertEquals(32.184, lsk.getDeltaTa(), 0.0);
    assertEquals(1.657E-3, lsk.getK(), 0.0);
    assertEquals(1.671E-2, lsk.getEB(), 0.0);
    double[] expected = new double[] {6.239996E0, 1.99096871E-7};
    double[] result = lsk.getM(new double[2]);
    assertNotSame(expected, result);
    assertTrue(Arrays.equals(expected, result));
  }

  @Test(expected = LSKInstantiationException.class)
  public void testCreateLSKBadKernelContentException() throws Exception {
    factory.createLSK(badPool);
  }

}
