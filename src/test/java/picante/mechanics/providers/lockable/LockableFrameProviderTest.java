package picante.mechanics.providers.lockable;

import static picante.mechanics.FrameTestCodes.AA;
import static picante.mechanics.FrameTestCodes.XB;
import java.util.List;
import org.junit.Test;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameProvider;
import picante.mechanics.FrameProviderTest;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;

/**
 * Test case that exercises the reference implementation of the frame provider.
 * <p>
 * Since many of the generic tests were written with this specific implementation in mind, there
 * isn't much to do. We will exercise a handful of the specific exception subclasses generated by
 * the various classes supporting this provider's implementation.
 * </p>
 */
public class LockableFrameProviderTest extends FrameProviderTest {

  @Override
  public FrameProvider createProvider(List<? extends FrameTransformFunction> sources) {
    return new LockableFrameProvider(sources, LockType.FUNCTION);
  }

  @Test(expected = LockableFrameLinkEvaluationException.class)
  public void testFrameTransformReferenceLinkEvalException() {

    FrameTransformFunction function = staticTreePureStateTransformProvider
        .createFrameTransformFunction(AA, XB, Coverage.ALL_TIME);

    function.getTransform(0.0, new RotationMatrixIJK());
  }

  @Test(expected = LockableFrameLinkEvaluationException.class)
  public void testStateTransformReferenceLinkEvalException() {

    StateTransformFunction function = staticTreePureStateTransformProvider
        .createStateTransformFunction(AA, XB, Coverage.ALL_TIME);

    function.getTransform(0.0, new RotationMatrixIJK());

  }

  @Test(expected = LockableFrameLinkEvaluationException.class)
  public void testStateTransformReferenceLinkStateEvalException() {

    StateTransformFunction function = staticTreePureStateTransformProvider
        .createStateTransformFunction(AA, XB, Coverage.ALL_TIME);

    function.getStateTransform(0.0, new StateTransform());

  }

}