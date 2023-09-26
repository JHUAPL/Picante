package picante.mechanics;

abstract class AbstractReversedStateTransformFunctionWrapper
    extends AbstractReversedFrameTransformFunctionWrapper implements StateTransformFunction {

  public AbstractReversedStateTransformFunctionWrapper(FrameTransformFunction function) {
    super(function);
  }

}
