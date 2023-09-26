package picante.spice.kernel.pck;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.eq;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.isA;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.data.list.Retrievable;
import picante.math.intervals.Interval;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;

public class PCKType2Test {

  private PCKType2 segment;
  private Retrievable<PCKType2Record> mock;

  @Before
  public void setUp() throws Exception {
    Retrievable<PCKType2Record> records = createMock(Retrievable.class);
    mock = records;
    segment = new PCKType2("NAME", 3000, 17, 0, 10000, 0, 5000, records);
  }

  @Test
  public void testGetType() {
    assertEquals(2, segment.getType());
  }

  @Test
  public void testGetTransformInitialTime() {
    expect(mock.get(eq(0), isA(PCKType2Record.class))).andReturn(null);
    expect(mock.size()).andReturn(2).anyTimes();
    replay(mock);
    segment.getTransform(0.0, new RotationMatrixIJK());
    verify(mock);
  }

  @Test
  public void testGetRecordList() {
    assertSame(mock, segment.getRecordList());
  }

  @Test
  public void testGetInitialEpoch() {
    assertEquals(0.0, segment.getInitialEpoch(), 0.0);
  }

  @Test
  public void testGetIntervalLength() {
    assertEquals(5000.0, segment.getIntervalLength(), 0.0);
  }

  @Test
  public void testGetBodyFrameID() {
    assertEquals(3000, segment.getBodyFrameID());
  }

  @Test
  public void testGetCoverage() {
    Coverage coverage = segment.getCoverage();
    Interval buffer = coverage.getBoundingInterval(new Interval());
    assertEquals(0, buffer.getBegin(), 0.0);
    assertEquals(10000, buffer.getEnd(), 0.0);
  }

  @Test
  public void testGetName() {
    assertEquals("NAME", segment.getName());
  }

  @Test
  public void testGetReferenceFrameID() {
    assertEquals(17, segment.getReferenceFrameID());
  }

}
