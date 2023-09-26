/*
 * Author : vandejd1 Created : May 2, 2011
 * 
 * Copyright (C) 2011 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.mechanics.utilities;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.FrameTransformFunction;

/**
 * Inverts the sense of the supplied frame transform function.
 * 
 * @author vandejd1
 */
@Deprecated
public class InverseFrameTransformFunction extends AbstractFrameTransformFunction
    implements FrameTransformFunction {
  private final FrameTransformFunction srcTransform;

  public InverseFrameTransformFunction(FrameTransformFunction srcTransform) {
    super(srcTransform.getToID(), srcTransform.getFromID(), srcTransform.getCoverage());
    this.srcTransform = srcTransform;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    return srcTransform.getTransform(time, buffer).transpose();
  }

}
