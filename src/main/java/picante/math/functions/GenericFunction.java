/**
 * Filename: GenericFunction.java Author : vandejd1 Created : Feb 25, 2009
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.math.functions;


/**
 * a definition for how functions in crucible are supposed to work.
 * 
 * The main idea in this pattern is that you supply a writable buffer to hold the result of the
 * evaluation. The idea is that memory management and the creation of these objects for the answer
 * is managed outside this function, which gives users of the function more control and avoids the
 * danger of returning class-ownede memory and also avoids the overhead of always creating a new
 * object.
 * 
 * NOTE: the main intent of this class is just to show how functions should generally be constructed
 * with crucible. It is not expected that every function will extend this interface, not will there
 * necissarily be a framework for dealing with generic functions at the level presented by this
 * interface.
 * 
 * For example, if you have a function where the independent variable is a primitive, then you might
 * not want to use this object-flavored approach because of the overhead of boxing the primitives
 * into object.
 * 
 * @author vandejd1
 */
public interface GenericFunction<IN, DEP> {
  /**
   * evaluate a function at a given independent variable value
   * 
   * @param x the independent variable at which the function is to be evaluated
   * @param writableBuffer the buffer to be filled with the nswer of the function evaluation
   * @return a reference to the writableBuffer
   */
  DEP getValue(IN x, DEP writableBuffer);
}
