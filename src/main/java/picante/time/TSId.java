/*
 * Filename: TSId.java Author : vandejd1 Created : Mar 20, 2009
 * 
 * Copyright (C) 2009 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import picante.exceptions.PicanteRuntimeException;

/**
 * This class serves as a way to identify a particular time system. The templated part allows you to
 * link a time system class with the the objects used to represent time in a particular time system.
 * 
 * TODO: The only place the template is being used is in the {@link TSManager} for type retrieval.
 * The retrieval method could be replaced with something akin to:
 * 
 * <pre>
 * <code>
 * 	&#64;SuppressWarnings("unchecked") 
 *  public <Q> ITimeSystem<Q> getKnownTimeSystem(TSId timeSystemId, Class<Q> timeType)
 *  {
 *  	ITimeSystem<Q> ts = (ITimeSystem<Q>)timeSystems.get(timeSystemId);
 *      if (ts == null) {
 *   		throw new UnknownTimeSystemException(timeSystemId+" is unknown.");
 * 		}
 * 		return ts;
 *  }
 * </code>
 * </pre>
 * 
 * @author vandejd1
 * @param <Q> Time system type
 * 
 */
@SuppressWarnings("unused")
public class TSId<Q> {

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    TSId<?> other = (TSId<?>) obj;
    if (id == null) {
      if (other.id != null) {
        return false;
      }
    } else if (!id.equals(other.id)) {
      return false;
    }
    return true;
  }

  private final String id;

  public TSId(String id) {
    if (id == null) {
      throw new PicanteRuntimeException("Error - null id not allowed in TSId constructor");
    }
    this.id = id;
  }

  @Override
  public String toString() {
    return id;
  }
}
