package picante.spice.kernelpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.List;
import java.util.Set;

class AssertionUtilities {

  /**
   * Verifies that the internal keyword value maps in pool are in a consistent state. Note: this
   * method makes assumptions about the implementation of BasicKernelPool that may be violated by
   * future reworkings of the code.
   * 
   * @param pool the pool to validate.
   */
  public static void assertPoolStateOK(UnwritableKernelPool pool) {

    for (String key : pool.getKeywords()) {

      if (pool.isDoubleValued(key) && pool.isStringValued(key)) {
        fail("Keyword: " + key + " is both numeric and string valued.");
      }

    }

  }

  /**
   * Asserts two BasicKernelPools are equal.
   * 
   * @param expected the expected pool
   * @param actual the actual pool to compare to the expected
   */
  public static void assertPoolEquality(UnwritableKernelPool expected,
      UnwritableKernelPool actual) {

    Set<String> expectedKeys = expected.getKeywords();
    Set<String> actualKeys = actual.getKeywords();

    for (String key : expectedKeys) {

      assertTrue("Missing expected keyword: " + key, actualKeys.contains(key));

      if (expected.isStringValued(key)) {

        assertTrue(
            "Keyword data content [" + key + "] must match; " + "found doubles expecting strings.",
            actual.isStringValued(key));

        /*
         * Check the string list content.
         */
        List<String> expectedValues = expected.getStrings(key);
        List<String> actualValues = actual.getStrings(key);

        assertEquals("Size of [" + key + "] does not match", expectedValues.size(),
            actualValues.size());

        for (int i = 0; i < expectedValues.size(); i++) {
          assertEquals("Value mismatch for [" + key + "] at index " + i, expectedValues.get(i),
              actualValues.get(i));
        }

      } else {

        assertTrue(
            "Keyword data content [" + key + "] must match; " + "found strings expecting doubles.",
            actual.isDoubleValued(key));

        /*
         * Check the string list content.
         */
        List<Double> expectedValues = expected.getDoubles(key);
        List<Double> actualValues = actual.getDoubles(key);

        assertEquals("Size of [" + key + "] does not match", expectedValues.size(),
            actualValues.size());

        for (int i = 0; i < expectedValues.size(); i++) {
          assertEquals("Value mismatch for [" + key + "] at index " + i, expectedValues.get(i),
              actualValues.get(i), 0.0);
        }

      }

    }

  }

}
