/**
 * Filename: ITimeFormatter.java Author : vandejd1 Created : Nov 24, 2008
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import java.util.List;


/**
 * 
 * @author vandejd1
 */
public interface IDelimitedTimeFormatter extends ITimeFormatter {
  List<String> getComponentNames();
}
