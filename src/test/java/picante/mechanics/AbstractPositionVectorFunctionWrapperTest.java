package picante.mechanics;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.utilities.SimpleEphemerisID;
import picante.mechanics.utilities.SimpleFrameID;

public class AbstractPositionVectorFunctionWrapperTest {

  PositionVectorFunction mock;
  SimpleEphemerisID ephemerisID;
  SimpleFrameID frameID;
  Coverage mockCoverage;
  PositionVectorFunction wrapper;

  PositionVectorFunction createMockToWrap() {
    return createMock(PositionVectorFunction.class);
  }

  PositionVectorFunction createWrapper() {
    return new AbstractPositionVectorFunctionWrapper(mock) {

      @Override
      @SuppressWarnings("unused")
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Before
  public void setUp() throws Exception {
    ephemerisID = new SimpleEphemerisID("EPHEMERIS");
    frameID = new SimpleFrameID("FRAME");
    mock = createMockToWrap();
    mockCoverage = createMock(Coverage.class);
    wrapper = createWrapper();
  }

  @Test
  public void testGetObserverID() {
    expect(mock.getObserverID()).andReturn(ephemerisID);
    replay(mock);
    assertSame(ephemerisID, wrapper.getObserverID());
  }

  @Test
  public void testGetTargetID() {
    expect(mock.getTargetID()).andReturn(ephemerisID);
    replay(mock);
    assertSame(ephemerisID, wrapper.getTargetID());
  }

  @Test
  public void testGetFrameID() {
    expect(mock.getFrameID()).andReturn(frameID);
    replay(mock);
    assertSame(frameID, wrapper.getFrameID());
  }

  @Test
  public void testGetCoverage() {
    expect(mock.getCoverage()).andReturn(mockCoverage);
    replay(mock);
    assertSame(mockCoverage, mock.getCoverage());
  }

}
