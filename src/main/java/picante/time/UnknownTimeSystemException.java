/**
 * Filename: UnknownTimeSystemException.java Author : vandejd1 Created : Mar 26, 2009
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import picante.exceptions.PicanteRuntimeException;

public class UnknownTimeSystemException extends PicanteRuntimeException {

  private static final long serialVersionUID = 1L;

  public UnknownTimeSystemException(String message) {
    super(message);
  }

}
