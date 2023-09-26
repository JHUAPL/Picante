package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

public class KeywordFormatterTest {

  private KeywordFormatter formatter;

  @Before
  public void setUp() throws Exception {
    formatter = new KeywordFormatter();
  }

  @Test
  public void testFormat() {
    /*
     * Just check that the formatter resets itself after each call.
     */
    assertEquals("TEST_5_FORMAT", formatter.format("TEST_%d_FORMAT", 5));
    assertEquals("NEW_4_TEST", formatter.format("NEW_%s_TEST", 4));
  }

}
