package picante.spice.kernel.tk.lsk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableSortedSet;
import picante.time.LeapsecondEntry;

public class LSKTest {

  private LSK lsk;

  // TODO: Add tests for the leapseconds table creation.
  @Before
  public void setUp() throws Exception {
    lsk = new LSK(32.184, 1.657E-3, 1.671E-2, new double[] {6.239996E0, 1.99096871E-7},
        ImmutableSortedSet.<LeapsecondEntry>of());
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void testLSK() {
    new LSK(32.184, 1.0, 1.0, new double[] {1.0}, ImmutableSortedSet.<LeapsecondEntry>of());
  }

  @Test
  public void testGetTDTProvider() {

    /*
     * Just kick the tires, as we already have unit tests for the provider.
     */
    TDTProvider p = (TDTProvider) lsk.getTDTProvider();

    assertEquals(1.657E-3, p.getK(), 0.0);
    assertEquals(1.671E-2, p.getEB(), 0.0);
    double[] expected = new double[] {6.239996E0, 1.99096871E-7};
    double[] result = p.getM(new double[2]);
    assertNotSame(result, expected);
    assertTrue(Arrays.equals(expected, result));

  }

  @Test
  public void testGetDeltaTa() {
    assertEquals(32.184, lsk.getDeltaTa(), 0.0);
  }

  @Test
  public void testGetK() {
    assertEquals(1.657E-3, lsk.getK(), 0.0);
  }

  @Test
  public void testGetEB() {
    assertEquals(1.671E-2, lsk.getEB(), 0.0);
  }

  @Test
  public void testGetM() {
    double[] expected = new double[] {6.239996E0, 1.99096871E-7};
    double[] result = lsk.getM(new double[2]);
    assertTrue(Arrays.equals(expected, result));
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void testGetMException() {
    lsk.getM(new double[1]);
  }

}
