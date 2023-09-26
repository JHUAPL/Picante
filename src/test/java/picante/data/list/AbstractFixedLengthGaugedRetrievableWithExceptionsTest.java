package picante.data.list;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

public class AbstractFixedLengthGaugedRetrievableWithExceptionsTest {

  private TestTable basic;
  private TestTable repeating;

  @Before
  public void setUp() throws Exception {
    List<Double> times = new ArrayList<Double>();
    times.add(0.0);
    times.add(1.0);
    times.add(2.0);

    List<StringBuilder> strings = new ArrayList<StringBuilder>();
    strings.add(new StringBuilder("TEST"));
    strings.add(new StringBuilder("TEST_TWO"));
    strings.add(new StringBuilder("TEST_THREE"));

    basic = new TestTable(times, strings);

    times = new ArrayList<Double>();
    times.add(0.0);
    times.add(1.0);
    times.add(1.0);

    strings = new ArrayList<StringBuilder>();
    strings.add(new StringBuilder("TEST_A"));
    strings.add(new StringBuilder("TEST_B"));
    strings.add(new StringBuilder("TEST_C"));

    repeating = new TestTable(times, strings);

  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTimeBasicLowerBoundException() {
    basic.getGauge(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTimeBasicUpperBoundException() {
    basic.getGauge(3);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTimeRepeatingLowerBoundException() {
    repeating.getGauge(-1);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetTimeRepeatingUpperBoundException() {
    repeating.getGauge(3);
  }

  @Test
  public void testGetTime() {
    assertEquals(0.0, basic.getGauge(0), 0.0);
    assertEquals(0.0, repeating.getGauge(0), 0.0);
    assertEquals(2.0, basic.getGauge(2), 0.0);
    assertEquals(1.0, repeating.getGauge(2), 0.0);
  }

  @Test
  public void testGetLength() {
    assertEquals(3, basic.size());
    assertEquals(3, repeating.size());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordBasicLowerBoundException() {
    basic.get(-1, new StringBuilder());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordBasicUpperBoundException() {
    basic.get(3, new StringBuilder());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordRepeatingLowerBoundException() {
    repeating.get(-1, new StringBuilder());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordRepeatingUpperBoundException() {
    repeating.get(3, new StringBuilder());
  }

  @Test
  public void testGetRecord() {
    assertEquals("TEST", basic.get(0, new StringBuilder()).toString());
    assertEquals("TEST_A", repeating.get(0, new StringBuilder()).toString());
    assertEquals("TEST_THREE", basic.get(2, new StringBuilder()).toString());
    assertEquals("TEST_C", repeating.get(2, new StringBuilder()).toString());
  }

  static class TestTable extends AbstractFixedLengthGaugedRetrievableWithExceptions<StringBuilder> {

    private List<Double> times;
    private List<StringBuilder> strings;

    public TestTable(List<Double> times, List<StringBuilder> strings) {
      super(times.size());
      this.times = times;
      this.strings = strings;
    }

    @Override
    protected StringBuilder obtainRecord(int index, StringBuilder buffer) {
      buffer.delete(0, buffer.length());
      buffer.append(strings.get(index));
      return buffer;
    }

    @Override
    protected double obtainTime(int index) {
      return times.get(index);
    }

  }

}
