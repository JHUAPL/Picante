package picante.mechanics;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.utilities.SimpleFrameID;

public class AbstractReversedFrameTransformFunctionWrapperTest {

  FrameTransformFunction mock;
  SimpleFrameID frameID;
  Coverage mockCoverage;
  FrameTransformFunction wrapper;

  FrameTransformFunction createMockToWrap() {
    return createMock(FrameTransformFunction.class);
  }

  @SuppressWarnings("unused")
  FrameTransformFunction createWrapper() {
    return new AbstractReversedFrameTransformFunctionWrapper(mock) {

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Before
  public void setUp() throws Exception {
    frameID = new SimpleFrameID("FRAME");
    mock = createMockToWrap();
    mockCoverage = createMock(Coverage.class);
    wrapper = createWrapper();
  }

  @Test
  public void testGetFromID() {
    expect(mock.getToID()).andReturn(frameID);
    replay(mock);
    assertSame(frameID, wrapper.getFromID());
  }

  @Test
  public void testGetToID() {
    expect(mock.getFromID()).andReturn(frameID);
    replay(mock);
    assertSame(frameID, wrapper.getToID());
  }

  @Test
  public void testGetCoverage() {
    expect(mock.getCoverage()).andReturn(mockCoverage);
    replay(mock);
    assertSame(mockCoverage, mock.getCoverage());
  }

}
