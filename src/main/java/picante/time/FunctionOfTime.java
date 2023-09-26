/*
 * Author : brownle1 Created : Mar 16, 2011
 * 
 * Copyright (C) 2011 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

/**
 * An interface for capturing a function that depends on time, where time is expressed using
 * {@link TSEpoch}
 * 
 * @author brownle1
 */
public interface FunctionOfTime {
  double evaluate(TSEpoch time);

}
