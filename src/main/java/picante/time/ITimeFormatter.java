/**
 * Filename: ITimeFormatter.java Author : vandejd1 Created : Nov 29, 2008
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

/**
 * 
 * @author vandejd1
 */
public interface ITimeFormatter {
  String getFormattedTime(UTCEpoch utc);
}
