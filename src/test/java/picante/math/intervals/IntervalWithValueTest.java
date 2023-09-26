package picante.math.intervals;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

public class IntervalWithValueTest {
  private IntervalWithValue intervalA;
  private IntervalWithValue intervalB;
  private IntervalWithValue tmp = new IntervalWithValue(5, 50, Math.sqrt(5 * 50));

  @Before
  public void setUp() throws Exception {
    intervalA = new IntervalWithValue(10, 20, 15);

    intervalB = new IntervalWithValue(tmp);
  }

  @Test
  public void testIntervalWithMid() {
    assertEquals(intervalA.getMidValue(), 15, 0);
    assertEquals(intervalA.getBegin(), 10, 0);
    assertEquals(intervalA.getEnd(), 20, 0);

    assertEquals(intervalB.getInterval().getBegin(), 5, 0);
    assertEquals(intervalB.getMidValue(), 15.8113883, 0.000001);
  }


}
