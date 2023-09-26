package picante.mechanics;


abstract class AbstractReversedFrameTransformFunctionWrapper implements FrameTransformFunction {

  private final FrameTransformFunction function;

  public AbstractReversedFrameTransformFunctionWrapper(FrameTransformFunction function) {
    super();
    this.function = function;
  }

  @Override
  public FrameID getFromID() {
    return function.getToID();
  }

  @Override
  public FrameID getToID() {
    return function.getFromID();
  }

  @Override
  public Coverage getCoverage() {
    return function.getCoverage();
  }

}
