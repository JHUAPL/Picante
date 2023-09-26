/**
 * Author : vandejd1 Created : May 6, 2010
 * 
 * Copyright (C) 2010 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.mechanics.utilities;

import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;

/**
 * Abstract implementation of the {@link FrameTransformFunction} interface that provides coverage
 * and ID access via references provided to the constructor.
 * 
 * @author vandejd1
 */
public abstract class AbstractFrameTransformFunction implements FrameTransformFunction {
  private final Coverage coverage;
  private final FrameID fromFrame;
  private final FrameID toFrame;

  /**
   * Creates the abstract function by providing references to capture
   * 
   * @param fromFrame the frame ID which this transforms vectors from
   * @param toFrame the frame ID to which this transforms vectors to
   * @param coverage the coverage over which this transform applies
   */
  public AbstractFrameTransformFunction(FrameID fromFrame, FrameID toFrame, Coverage coverage) {
    this.fromFrame = fromFrame;
    this.toFrame = toFrame;
    this.coverage = coverage;
  }

  @Override
  public Coverage getCoverage() {
    return coverage;
  }

  @Override
  public FrameID getFromID() {
    return fromFrame;
  }

  @Override
  public FrameID getToID() {
    return toFrame;
  }

}
