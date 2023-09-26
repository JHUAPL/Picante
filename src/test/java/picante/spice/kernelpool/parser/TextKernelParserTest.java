package picante.spice.kernelpool.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import picante.spice.kernelpool.BasicKernelPool;

/**
 * J-Unit test framework for TextKernelParser.
 * 
 * This test suite exercises the convenience access methods placed into the JavaCC generated parser.
 * 
 *          TODO: adjust these tests to support the fact that the TextKernelParser is now linked to
 *          the time parser I've been developing.
 */
public class TextKernelParserTest {

  private static final double EPSILON = 1.0E-14;

  private static final String normBuffer =
      "This is a simple test buffer for the TextKernelParser to\n" + "successfully digest.\n"
          + "   \\begindata\n" + "   A = ( 1.0, 2.0, 3.0 )\n" + "   a = ( '1.0' '2.0' '3.0' )\n"
          + "   A += ( 4.0 5.0 6.0 )\n" + "   a += ( '4.0', '5.0', '6.0')\n" + "   test = -1.57\n"
          + "   anotherTest = '''a''b''c'''\n";

  private static final String dateBuffer =
      "This simple text buffer contains a single keyword value assignment\n"
          + "pair that defines a single @date style expansion.\n" + "\\begindata\n"
          + "   A = @2005-JAN-10-12:00:00\n";

  private static final String badBuffer =
      "This is a simple text buffer that should cause the parser to\n"
          + "explode due to formatting errors.\n" + "\\begindata\n" + "  TESTING 1..2..3?\n";

  private final TextKernelParser parser = new TextKernelParser();

  @Test
  public void testNoLastNewLineKernel() throws Exception {

    BasicKernelPool pool =
        parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("nonewline.tk")));

    BasicKernelPool validationPool = new BasicKernelPool();
    validationPool.addIntegers("A", ImmutableList.of(5));
    validationPool.addStrings("\\begintextABCD", ImmutableList.of("A"));

    comparePools(validationPool, pool);

  }

  @Test(expected = ParseException.class)
  public void testNoLastNewLineKernelBadFile() throws Exception {
    parser
        .parse(new InputStreamReader(this.getClass().getResourceAsStream("nonewline_badfile.tk")));
  }

  @Test
  public void testNoLastNewLineBeginDataKernel() throws Exception {

    BasicKernelPool pool = parser.parse(
        new InputStreamReader(this.getClass().getResourceAsStream("nonewline_begindata.tk")));

    BasicKernelPool validationPool = new BasicKernelPool();
    validationPool.addIntegers("A", ImmutableList.of(5));
    validationPool.addStrings("\\begintextABCD", ImmutableList.of("A"));

    comparePools(validationPool, pool);

  }


  @Test
  public void testBegindataAssignmentKernel() throws Exception {

    BasicKernelPool pool = parser.parse(
        new InputStreamReader(this.getClass().getResourceAsStream("begindata_assignment.tk")));

    BasicKernelPool validationPool = new BasicKernelPool();
    validationPool.addIntegers("A", ImmutableList.of(5));
    validationPool.addStrings("\\begintextABCD", ImmutableList.of("A"));
    validationPool.addIntegers("\\begindata", ImmutableList.of(50));

    comparePools(validationPool, pool);

  }


  @Test
  public void testNAIFKernels() {

    BasicKernelPool pool;
    BasicKernelPool validationPool;

    try {
      pool =
          parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("naif0008.tls")));

      validationPool =
          parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("naif0008.bls")));

      comparePools(validationPool, pool);

      pool =
          parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("cas00093.tsc")));

      validationPool =
          parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("cas00093.bsc")));

      comparePools(validationPool, pool);

      pool = parser
          .parse(new InputStreamReader(this.getClass().getResourceAsStream("messenger_154.tsc")));
      validationPool = parser
          .parse(new InputStreamReader(this.getClass().getResourceAsStream("messenger_154.bsc")));

      comparePools(validationPool, pool);

      pool =
          parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("pck00008.tpc")));
      validationPool =
          parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("pck00008.bpc")));

      comparePools(validationPool, pool);

      pool = parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("cas_v39.tf")));
      validationPool =
          parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("cas_v39.bf")));

      comparePools(validationPool, pool);

      pool = parser.parse(new InputStreamReader(
          this.getClass().getResourceAsStream("MRO_SCLKSCET.00006.65536.tsc")));
      validationPool = parser.parse(new InputStreamReader(
          this.getClass().getResourceAsStream("MRO_SCLKSCET.00006.65536.bsc")));

      comparePools(validationPool, pool);

    } catch (ParseException pe) {
      fail("Unexpected ParseException thrown.");
    }
  }

  /**
   * This test *should* pass, but due to a failure on our part to recognize constructs such as this:
   * 
   * <pre>
   *    KEYWORD = 'A' 'B' 'C' ...
   * </pre>
   * 
   * as being valid, it fails. Granted this is a bit pathological, as all SPICE text kernels I have
   * ever seen bracket multiple values in ( ).
   */
  @Test
  public void testLROKernel() throws Exception {
    BasicKernelPool pool;
    BasicKernelPool validationPool;

    pool = parser.parse(
        new InputStreamReader(this.getClass().getResourceAsStream("lro_frames_2010036_v01.tf")));
    validationPool = parser.parse(
        new InputStreamReader(this.getClass().getResourceAsStream("lro_frames_2010036_v01.bf")));

    comparePools(validationPool, pool);

  }

  @Test
  public void testNoNewLineKernel() throws Exception {
    BasicKernelPool pool;
    BasicKernelPool validationPool;

    pool = parser.parse(
        new InputStreamReader(this.getClass().getResourceAsStream("naif0008_nonewline.tls")));
    validationPool =
        parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("naif0008.bls")));

    comparePools(validationPool, pool);

  }

  @Test
  public void testNoNewLineAlternateKernel() throws Exception {
    BasicKernelPool pool;
    BasicKernelPool validationPool;

    pool = parser.parse(
        new InputStreamReader(this.getClass().getResourceAsStream("naif0008_nonewlinealt.tls")));
    validationPool =
        parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("naif0008.bls")));

    comparePools(validationPool, pool);

  }

  @Test
  public void testPiKernel() throws Exception {

    BasicKernelPool pool;
    BasicKernelPool validationPool;

    pool = parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("pi.tk")));
    validationPool =
        parser.parse(new InputStreamReader(this.getClass().getResourceAsStream("pi.bk")));

    comparePools(validationPool, pool);

  }

  /*
   * Test method for 'kernelpool.parser.parser.parse(Reader)'
   */
  @Test
  public void testParse() {

    BasicKernelPool thePool;
    List<Double> doubles;
    List<String> strings;

    /*
     * Try and parse the @date expansion buffer. Given the fact that the time string parser is not
     * implemented, this should result in the parser throwing a ParseException.
     */
    try {
      thePool = parser.parse(new StringReader(dateBuffer));

      doubles = thePool.getDoubles("A");

      assertNotNull(doubles);
      assertEquals(1, doubles.size());
      assertEquals(158630400.0, doubles.get(0), 0.0);

    } catch (ParseException pe) {
      fail("Unexpected ParseException was thrown.");
    }

    /*
     * Test that the badly formatted buffer generates an expected ParseException.
     */
    try {
      thePool = parser.parse(new StringReader(badBuffer));
      fail("Expected ParseException was not thrown.");
    } catch (ParseException pe) {
      /*
       * Who knows exactly what the parser is going to say about the problem, so we'll just leave
       * this block of code empty.
       */
    }

    /*
     * Check that a normal text kernel buffer parses properly into a KernelPool object.
     */
    try {
      thePool = parser.parse(new StringReader(normBuffer));

      doubles = thePool.getDoubles("A");

      assertNotNull(doubles);
      assertEquals(6, doubles.size());
      assertEquals(1.0, doubles.get(0), 0.0);
      assertEquals(2.0, doubles.get(1), 0.0);
      assertEquals(3.0, doubles.get(2), 0.0);
      assertEquals(4.0, doubles.get(3), 0.0);
      assertEquals(5.0, doubles.get(4), 0.0);
      assertEquals(6.0, doubles.get(5), 0.0);

      strings = thePool.getStrings("a");

      assertNotNull(strings);
      assertEquals(6, strings.size());
      assertEquals("1.0", strings.get(0));
      assertEquals("2.0", strings.get(1));
      assertEquals("3.0", strings.get(2));
      assertEquals("4.0", strings.get(3));
      assertEquals("5.0", strings.get(4));
      assertEquals("6.0", strings.get(5));

      doubles = thePool.getDoubles("test");

      assertNotNull(doubles);
      assertEquals(1, doubles.size());
      assertEquals(-1.57, doubles.get(0), 0.0);

      strings = thePool.getStrings("anotherTest");

      assertNotNull(strings);
      assertEquals(1, strings.size());
      assertEquals("'a'b'c'", strings.get(0));
    } catch (ParseException pe) {
      fail("Unexpected ParseException generated.");
    }
  }

  /*
   * Test method for 'kernelpool.parser.parser.parseInto(KernelPool, Reader)'
   */
  @Test
  public void testParseInto() {

    List<Double> doubles;
    List<String> strings;

    /*
     * Create an empty KernelPool for the exception tests.
     */
    BasicKernelPool thePool = new BasicKernelPool();

    /*
     * Parse the dateBuffer into this new pool. It should succeed.
     */
    try {
      parser.parseInto(thePool, new StringReader(dateBuffer));
      doubles = thePool.getDoubles("A");

      assertNotNull(doubles);
      assertEquals(1, doubles.size());
      assertEquals(158630400.0, doubles.get(0), 0.0);

    } catch (ParseException pe) {
      fail("Unexpected ParseException was thrown.");
    }

    /*
     * Next attempt to parse the ill-formed buffer. This too should generate a ParseException.
     */
    try {
      parser.parseInto(thePool, new StringReader(badBuffer));
      fail("Expected ParseException was not thrown.");
    } catch (ParseException pe) {
      /*
       * Who knows exactly what the parser is going to say about the problem, so we'll just leave
       * this block of code empty.
       */
    }

    /*
     * Create a new KernelPool object. Insert some values that our standard buffer will definitely
     * replace.
     */
    thePool = new BasicKernelPool();

    doubles = new ArrayList<Double>();
    doubles.add(-1.0);
    doubles.add(-2.0);

    strings = new ArrayList<String>();
    strings.add("ABC");

    thePool.addDoubles("A", doubles);
    thePool.addStrings("NOT_IN_BUFFER", strings);

    /*
     * Check that the add was successful.
     */
    doubles = thePool.getDoubles("A");

    assertNotNull(doubles);

    /*
     * Now add the contents of normBuffer into this KernelPool.
     */
    try {
      parser.parseInto(thePool, new StringReader(normBuffer));

      doubles = thePool.getDoubles("A");

      assertNotNull(doubles);
      assertEquals(6, doubles.size());
      assertEquals(1.0, doubles.get(0), 0.0);
      assertEquals(2.0, doubles.get(1), 0.0);
      assertEquals(3.0, doubles.get(2), 0.0);
      assertEquals(4.0, doubles.get(3), 0.0);
      assertEquals(5.0, doubles.get(4), 0.0);
      assertEquals(6.0, doubles.get(5), 0.0);

      strings = thePool.getStrings("a");

      assertNotNull(strings);
      assertEquals(6, strings.size());
      assertEquals("1.0", strings.get(0));
      assertEquals("2.0", strings.get(1));
      assertEquals("3.0", strings.get(2));
      assertEquals("4.0", strings.get(3));
      assertEquals("5.0", strings.get(4));
      assertEquals("6.0", strings.get(5));

      doubles = thePool.getDoubles("test");

      assertNotNull(doubles);
      assertEquals(1, doubles.size());
      assertEquals(-1.57, doubles.get(0), 0.0);

      strings = thePool.getStrings("anotherTest");

      assertNotNull(strings);
      assertEquals(1, strings.size());
      assertEquals("'a'b'c'", strings.get(0));

      /*
       * Lastly check that NOT_IN_BUFFER is still in the pool, and that its contents have not been
       * altered by the load.
       */
      strings = thePool.getStrings("NOT_IN_BUFFER");

      assertNotNull(strings);
      assertEquals(1, strings.size());
      assertEquals("ABC", strings.get(0));

    } catch (ParseException pe) {
      fail("Unexpected ParseException was thrown.");
    }
  }

  /**
   * Compares two KernelPool objects for equality.
   * 
   * @param expected the pool containing the expected values.
   * @param actual the pool containing the values to test.
   */
  private void comparePools(BasicKernelPool expected, BasicKernelPool actual) {
    /*
     * First check that the key sets from both pools are the same.
     */
    Set<String> eKeys = expected.getKeywords();
    Set<String> aKeys = actual.getKeywords();

    assertEquals(eKeys.size(), aKeys.size());

    /*
     * Now iterate over the key lists, testing that they are the same.
     */
    for (String key : eKeys) {
      assertTrue(aKeys.contains(key));

      assertEquals(expected.isDoubleValued(key), actual.isDoubleValued(key));

      assertEquals(expected.isStringValued(key), actual.isStringValued(key));
    }

    /*
     * And now compare the data content of each of the keys.
     */
    for (String key : eKeys) {

      if (expected.isDoubleValued(key)) {
        List<Double> aList = actual.getDoubles(key);
        List<Double> eList = expected.getDoubles(key);

        for (int i = 0; i < eList.size(); i++) {

          if (aList.get(i) != 0.0) {
            assertEquals(0.0, eList.get(i) / aList.get(i) - 1.0, EPSILON);
          } else {
            assertEquals(key + " at index: " + eList.size() + " " + aList.size(), eList.get(i),
                aList.get(i), EPSILON);
          }
        }

      } else {
        List<String> aList = actual.getStrings(key);
        List<String> eList = expected.getStrings(key);

        for (int i = 0; i < eList.size(); i++) {
          assertEquals(eList.get(i), aList.get(i));
        }
      }
    }
  }

}
