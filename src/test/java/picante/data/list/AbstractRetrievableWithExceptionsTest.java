package picante.data.list;

import static org.easymock.EasyMock.capture;
import static org.easymock.EasyMock.createMockBuilder;
import static org.easymock.EasyMock.eq;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.junit.CaptureAndAnswer;

public class AbstractRetrievableWithExceptionsTest {

  private AbstractRetrievableWithExceptions<StringBuffer> retrievable;

  @Before
  public void setUp() throws Exception {
    retrievable = createMockBuilder(AbstractRetrievableWithExceptions.class).createMock();
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetLessThanZeroIndexException() {
    expect(retrievable.size()).andReturn(3).anyTimes();
    replay(retrievable);
    retrievable.get(-1, new StringBuffer());
    verify(retrievable);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIndexTooLargeException() {
    expect(retrievable.size()).andReturn(3).anyTimes();
    replay(retrievable);
    retrievable.get(3, new StringBuffer());
    verify(retrievable);
  }

  @Test(expected = IndexOutOfBoundsException.class)
  public void testGetIndexWayTooLargeException() {
    expect(retrievable.size()).andReturn(3).anyTimes();
    replay(retrievable);
    retrievable.get(4, new StringBuffer());
    verify(retrievable);
  }

  @Test
  public void testGet() {

    expect(retrievable.size()).andReturn(3).anyTimes();

    CaptureAndAnswer<StringBuffer> capAndAns = new CaptureAndAnswer<StringBuffer>() {

      @Override
      public void set(StringBuffer captured) {
        captured.setLength(0);
        captured.append("TEST");
      }
    };
    expect(retrievable.obtain(eq(2), capture(capAndAns.getCapture()))).andAnswer(capAndAns);
    replay(retrievable);

    StringBuffer buffer = new StringBuffer();
    StringBuffer result = retrievable.get(2, buffer);

    assertSame(result, buffer);
    assertEquals("TEST", result.toString());

    verify(retrievable);

  }

}
