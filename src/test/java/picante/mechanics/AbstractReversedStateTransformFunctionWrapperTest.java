package picante.mechanics;

import static org.easymock.EasyMock.createMock;
import org.junit.Before;
import picante.math.vectorspace.RotationMatrixIJK;

public class AbstractReversedStateTransformFunctionWrapperTest
    extends AbstractReversedFrameTransformFunctionWrapperTest {

  @Override
  FrameTransformFunction createMockToWrap() {
    return createMock(StateTransformFunction.class);
  }

  @SuppressWarnings("unused")
  @Override
  FrameTransformFunction createWrapper() {
    return new AbstractReversedStateTransformFunctionWrapper(mock) {

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        throw new UnsupportedOperationException();
      }

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {
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
