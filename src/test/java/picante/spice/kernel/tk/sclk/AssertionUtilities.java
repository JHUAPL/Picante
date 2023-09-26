package picante.spice.kernel.tk.sclk;

import static org.junit.Assert.assertArrayEquals;

public class AssertionUtilities {

  public static void assertFieldEquality(SCLK expected, SCLK actual) {
    assertArrayEquals(
        expected.getFields(expected.getNumberOfFields(), new long[expected.getNumberOfFields()]),
        actual.getFields(actual.getNumberOfFields(), new long[actual.getNumberOfFields()]));
  }

}
