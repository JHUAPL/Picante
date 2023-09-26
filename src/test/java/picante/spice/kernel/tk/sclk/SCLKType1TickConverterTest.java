package picante.spice.kernel.tk.sclk;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static picante.spice.kernel.tk.sclk.AssertionUtilities.assertFieldEquality;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;

public class SCLKType1TickConverterTest {

  private SCLKType1TickConverter singleFieldConverter;
  private SCLKType1TickConverter converter;
  private SCLK sclk = new SCLK();

  @Before
  public void setUp() throws Exception {

    singleFieldConverter =
        new SCLKType1TickConverter(-100, Arrays.asList(10000000.), Arrays.asList(1.));
    converter = new SCLKType1TickConverter(-101, Arrays.asList(10000000., 150., 50., 1000.),
        Arrays.asList(0., 1., 5., 10.));

  }

  @Test(expected = SCLKInstantiationException.class)
  public void constructorArgumentLengthMismatch() throws Exception {
    new SCLKType1TickConverter(-102, Arrays.asList(1., 2.), Arrays.asList(0.));
  }

  @Test
  public void testCreateTicksPerField() {
    double[] expected = new double[] {1.0};
    assertArrayEquals(expected,
        SCLKType1TickConverter.createTicksPerField(Arrays.asList(10000000.)), 0.0);
    expected = new double[] {7500000.0, 50000, 1000, 1};
    assertArrayEquals(expected,
        SCLKType1TickConverter.createTicksPerField(Arrays.asList(10000000., 150., 50., 1000.)),
        0.0);
  }

  @Test
  public void testCreateOffsets() {
    double[] expected = new double[] {1.0};
    assertArrayEquals(expected, SCLKType1TickConverter.createOffsets(Arrays.asList(1.)), 0.0);
    expected = new double[] {0, 1, 5, 10};
    assertArrayEquals(expected,
        SCLKType1TickConverter.createOffsets(Arrays.asList(0., 1., 5., 10.)), 0.0);
  }

  @Test(expected = SCLKEvaluationException.class)
  public void testConvertToTicksBadSCLKException() {
    singleFieldConverter.convertToTicks(new SCLK(1, -1));
  }

  @Test
  public void testGetNumberOfFields() {
    assertEquals(4, converter.getNumberOfFields());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTicksPerFieldLowException() {
    converter.getTicksPerField(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTicksPerFieldHighException() {
    converter.getTicksPerField(4);
  }

  @Test
  public void testGetTicksPerField() {
    assertEquals(7500000.0, converter.getTicksPerField(0), 0.0);
    assertEquals(50000.0, converter.getTicksPerField(1), 0.0);
    assertEquals(1000.0, converter.getTicksPerField(2), 0.0);
    assertEquals(1.0, converter.getTicksPerField(3), 0.0);

  }


  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetOffsetLowException() {
    converter.getOffset(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetOffsetHighException() {
    converter.getOffset(4);
  }

  @Test
  public void testGetOffset() {
    assertEquals(0.0, converter.getOffset(0), 0.0);
    assertEquals(1.0, converter.getOffset(1), 0.0);
    assertEquals(5.0, converter.getOffset(2), 0.0);
    assertEquals(10.0, converter.getOffset(3), 0.0);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetFieldModulusLowException() {
    converter.getFieldModulus(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetFieldModulusHighException() {
    converter.getFieldModulus(4);
  }

  @Test
  public void testGetFieldModulus() {
    assertEquals(10000000.0, converter.getFieldModulus(0), 0.0);
    assertEquals(150.0, converter.getFieldModulus(1), 0.0);
    assertEquals(50.0, converter.getFieldModulus(2), 0.0);
    assertEquals(1000.0, converter.getFieldModulus(3), 0.0);
  }

  @Test
  public void testConvertToTicks() {

    /*
     * The single field converter simply subtracts 1 from the field value.
     */
    sclk.setFields(501231231);
    assertEquals(501231230.0, singleFieldConverter.convertToTicks(sclk), 0.0);

    sclk.setFields(501231230001L);
    assertEquals(501231230000.0, singleFieldConverter.convertToTicks(sclk), 0.0);

    /*
     * Test the multiple field converter.
     */
    sclk.setFields(0, 1, 5, 10);
    assertEquals(0.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(1, 1, 5, 10);
    assertEquals(7500000.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100, 1, 5, 10);
    assertEquals(750000000.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100, 1, 5, 1000);
    assertEquals(750000990.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100, 1, 5, 1010);
    assertEquals(750001000.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100, 1, 5, 1011);
    assertEquals(750001001.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100, 1, 25, 1011);
    assertEquals(750021001.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100, 100, 25, 1011);
    assertEquals(754971001.0, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100000000, 100, 25, 1011);
    assertEquals(750000004971001.0, converter.convertToTicks(sclk), 0.0);

    /*
     * Select values that drive things into the double precision round-off regime.
     */
    sclk.setFields(100000000000L, 9991234, 25, 1011);
    assertEquals(7.50000499561671040E+017, converter.convertToTicks(sclk), 0.0);

    sclk.setFields(100000000000L, 9991234, 25, 91);
    assertEquals(7.50000499561670144E+017, converter.convertToTicks(sclk), 0.0);

  }

  @Test(expected = SCLKEvaluationException.class)
  public void testRemoveOffsetException() {
    sclk.setFields(0, 1, 5, 9);
    converter.removeOffset(sclk, 3);
  }

  @Test
  public void testRemoveOffset() {
    sclk.setFields(100, 101, 102, 103);
    assertEquals(100.0, converter.removeOffset(sclk, 0), 0.0);
    assertEquals(100.0, converter.removeOffset(sclk, 1), 0.0);
    assertEquals(97.0, converter.removeOffset(sclk, 2), 0.0);
    assertEquals(93.0, converter.removeOffset(sclk, 3), 0.0);
  }

  @Test
  public void testConvertToSCLK() {

    SCLK expected = new SCLK();

    expected.setFields(0, 1, 5, 10);
    assertFieldEquality(expected, converter.convertToSCLK(0.0, sclk));

    expected.setFields(0, 1, 5, 11);
    assertFieldEquality(expected, converter.convertToSCLK(1.0, sclk));

    expected.setFields(1, 1, 5, 10);
    assertFieldEquality(expected, converter.convertToSCLK(7500000.0, sclk));

    expected.setFields(100, 1, 5, 10);
    assertFieldEquality(expected, converter.convertToSCLK(750000000.0, sclk));

    expected.setFields(100, 1, 6, 10);
    assertFieldEquality(expected, converter.convertToSCLK(750001000.0, sclk));

    expected.setFields(100, 1, 6, 11);
    assertFieldEquality(expected, converter.convertToSCLK(750001001.0, sclk));

    expected.setFields(100, 1, 26, 11);
    assertFieldEquality(expected, converter.convertToSCLK(750021001.0, sclk));

    expected.setFields(100, 100, 26, 11);
    assertFieldEquality(expected, converter.convertToSCLK(754971001.0, sclk));

    expected.setFields(100000000, 100, 26, 11);
    assertFieldEquality(expected, converter.convertToSCLK(750000004971001.0, sclk));

    expected.setFields(100000066608L, 34, 26, 50);
    assertFieldEquality(expected, converter.convertToSCLK(7.50000499561671040E+017, sclk));

    expected.setFields(100000069440L, 121, 11, 794);
    assertFieldEquality(expected, converter.convertToSCLK(7.50000520806006784E+017, sclk));

  }

  @Test
  public void testIsValidClock() {

    sclk.setFields(0, 1, 5, 10);
    assertTrue(converter.isValidClock(sclk));

    sclk.setFields(1, 1, 5, 10);
    assertTrue(converter.isValidClock(sclk));

    sclk.setFields(0, 1);
    assertTrue(converter.isValidClock(sclk));

    sclk.setFields(0, 0);
    assertFalse(converter.isValidClock(sclk));

    sclk.setFields(0, 1, 5, 10, 100);
    assertFalse(converter.isValidClock(sclk));

    sclk.setFields(-1, 1, 5, 10);
    assertFalse(converter.isValidClock(sclk));

    sclk.setFields(0, 0, 5, 10);
    assertFalse(converter.isValidClock(sclk));

    sclk.setFields(0, 1, 4, 10);
    assertFalse(converter.isValidClock(sclk));

    sclk.setFields(0, 1, 5, 9);
    assertFalse(converter.isValidClock(sclk));

  }

}
