package picante.mechanics;

import static org.easymock.EasyMock.createMock;
import org.junit.Before;
import picante.math.vectorspace.VectorIJK;

public class AbstractReversedStateVectorFunctionWrapperTest
    extends AbstractReversedPositionVectorFunctionWrapperTest {

  @Override
  @SuppressWarnings("unused")
  PositionVectorFunction createWrapper() {
    return new AbstractReversedStateVectorFunctionWrapper(mock) {

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        throw new UnsupportedOperationException();
      }

      @Override
      public StateVector getState(double time, StateVector buffer) {
        throw new UnsupportedOperationException();
      }
    };
  }

  @Override
  PositionVectorFunction createMockToWrap() {
    return createMock(StateVectorFunction.class);
  }

  @Override
  @Before
  public void setUp() throws Exception {
    super.setUp();
  }

}
