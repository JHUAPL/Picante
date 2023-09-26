package picante.mechanics;

import static org.easymock.EasyMock.createMock;
import org.junit.Before;
import picante.math.vectorspace.VectorIJK;

public class AbstractStateVectorFunctionWrapperTest
    extends AbstractPositionVectorFunctionWrapperTest {

  @Override
  StateVectorFunction createMockToWrap() {
    return createMock(StateVectorFunction.class);
  }

  @Override
  @SuppressWarnings("unused")
  PositionVectorFunction createWrapper() {
    return new AbstractStateVectorFunctionWrapper(mock) {

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
  @Before
  public void setUp() throws Exception {
    super.setUp();
  }

}
