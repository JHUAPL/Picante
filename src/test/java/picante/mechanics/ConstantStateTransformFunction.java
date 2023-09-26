package picante.mechanics;

/**
 * Simple implementation of the <code>StateTransformFunction</code> interface that provides a
 * constant state transform over all time.
 */
public class ConstantStateTransformFunction extends ConstantFrameTransformFunction
    implements StateTransformFunction {

  private final StateTransform transform = new StateTransform();

  public ConstantStateTransformFunction(FrameTestCodes fromID, FrameTestCodes toID,
      Coverage coverage, UnwritableStateTransform transform) {
    super(fromID, toID, coverage, transform.getRotation());
    this.transform.setTo(transform);
  }

  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {

    if (!getCoverage().contains(time)) {
      throw new FrameEvaluationException();
    }

    return buffer.setTo(transform);
  }

}
