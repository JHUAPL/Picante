package picante.mechanics;

import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;
import picante.math.vectorspace.VectorIJK;

public class AbstractReversedPositionVectorFunctionWrapperTest
    extends AbstractPositionVectorFunctionWrapperTest {

  @Override
  @Before
  public void setUp() throws Exception {
    super.setUp();
  }

  @Override
  @SuppressWarnings("unused")
  PositionVectorFunction createWrapper() {
    return new AbstractReversedPositionVectorFunctionWrapper(mock) {

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Override
  @Test
  public void testGetObserverID() {
    expect(mock.getTargetID()).andReturn(ephemerisID);
    replay(mock);
    assertSame(ephemerisID, wrapper.getObserverID());
  }

  @Override
  @Test
  public void testGetTargetID() {
    expect(mock.getObserverID()).andReturn(ephemerisID);
    replay(mock);
    assertSame(ephemerisID, wrapper.getTargetID());
  }

}
