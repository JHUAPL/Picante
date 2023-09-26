package picante.spice.kernel.tk.lsk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import org.junit.Before;
import org.junit.Test;

/**
 * This class exercises the uniform time provider that maps TDB to TDT and back. It reads data
 * stored in two binary files included in the class path as resources that was generated with a
 * SPICE program.
 */
public class TDTProviderTest {

  private static final double TIGHT_EPSILON = 1E-17;

  private TDTProvider provider;

  @Before
  public void setUp() throws Exception {
    provider = new TDTProvider(1.657E-3, 1.671E-2, new double[] {6.239996E0, 1.99096871E-7});
  }

  @Test
  public void testTDTProvider() throws Exception {
    /*
     * Just check that the number of iterations was respected by the provider constructor.
     */
    TDTProvider iteratedProvider = new TDTProvider(1.0, 1.0, new double[2], 5);
    assertEquals(5, iteratedProvider.getIterations());
  }

  @Test
  public void testConvertToTDB() throws Exception {
    /*
     * Get the data from the test file for this case.
     */
    List<Double> testData = TestUtilities.getTestFileContents("tdttotdb.dat");

    /*
     * Check to make certain there is at least one element in the testData list.
     */
    if (testData.size() < 1) {
      fail("No test data obtained.");
    }

    /*
     * Loop through the test data, two doubles at a time checking for consistency and correctness.
     */
    ListIterator<Double> iterator = testData.listIterator();

    while (iterator.hasNext()) {
      double tdt = iterator.next();
      double expectedTDB = iterator.next();
      double et = provider.convertToTDB(tdt);
      assertEquals(expectedTDB, et, TIGHT_EPSILON);
    }

  }

  @Test
  public void testConvertToUniformTime() throws Exception {

    /*
     * Get the data from the test file for this case.
     */
    List<Double> testData = TestUtilities.getTestFileContents("tdbtotdt.dat");

    /*
     * Check to make certain there is at least one element in the testData list.
     */
    if (testData.size() < 1) {
      fail("No test data obtained.");
    }

    /*
     * Loop through the test data, two doubles at a time checking for consistency and correctness.
     */
    ListIterator<Double> iterator = testData.listIterator();

    while (iterator.hasNext()) {
      double et = iterator.next();
      double expectedTDT = iterator.next();
      double tdt = provider.convertToUniformTime(et);
      assertEquals(expectedTDT, tdt, TIGHT_EPSILON);
    }

  }

  @Test
  public void testGetK() {
    assertEquals(1.657E-3, provider.getK(), 0.0);
  }

  @Test
  public void testGetEB() {
    assertEquals(1.671E-2, provider.getEB(), 0.0);
  }

  @Test
  public void testGetM() {
    double[] expected = new double[] {6.239996E0, 1.99096871E-7};

    double[] actual = provider.getM(new double[2]);
    assertNotSame(expected, actual);

    assertTrue(Arrays.equals(expected, actual));
  }

  @Test
  public void testGetIterations() {
    assertEquals(3, provider.getIterations());
  }

}
