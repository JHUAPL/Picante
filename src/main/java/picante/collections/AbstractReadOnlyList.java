/**
 * Filename: AbstractReadOnlyList.java Author : vandejd1 Created : Feb 24, 2009
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.collections;

import java.util.List;
import java.util.RandomAccess;


/**
 * a base class that allows you to easily get a {@link List} view of objects
 * 
 * all add or modify methods throw runtime exceptions
 * 
 * this version implements the marker interface RandomAccess, so make sure your implementation
 * suppoorts this (but the only consequence is that Java collection utilities will be slow).
 * 
 * @author vandejd1
 */
public abstract class AbstractReadOnlyList<T> extends AbstractSequentialReadOnlyList<T>
    implements RandomAccess {

}
