package picante.data.list;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

public class AbstractFixedLengthRetrievableWithExceptionsTest {

  private TestList list;

  @Before
  public void setUp() throws Exception {
    List<StringBuilder> strings = new ArrayList<StringBuilder>();
    strings.add(new StringBuilder("TEST"));
    strings.add(new StringBuilder("TEST_TWO"));
    strings.add(new StringBuilder("TEST_THREE"));

    list = new TestList(strings);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordBasicLowerBoundException() {
    list.get(-1, new StringBuilder());
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetRecordBasicUpperBoundException() {
    list.get(3, new StringBuilder());
  }

  @Test
  public void testGetLength() {
    assertEquals(3, list.size());
  }

  @Test
  public void testGetRecord() {
    assertEquals("TEST", list.get(0, new StringBuilder()).toString());
    assertEquals("TEST_TWO", list.get(1, new StringBuilder()).toString());
    assertEquals("TEST_THREE", list.get(2, new StringBuilder()).toString());
  }

  static class TestList extends AbstractFixedLengthRetrievableWithExceptions<StringBuilder> {

    private List<StringBuilder> strings;

    public TestList(List<StringBuilder> strings) {
      super(strings.size());
      this.strings = strings;
    }

    @Override
    protected StringBuilder obtainRecord(int index, StringBuilder buffer) {
      buffer.delete(0, buffer.length());
      buffer.append(strings.get(index));
      return buffer;
    }
  }

}
