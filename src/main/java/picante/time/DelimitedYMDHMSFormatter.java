/**
 * Filename: YMDHMSFormatter.java Author : vandejd1 Created : Nov 25, 2008
 * 
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import java.util.ArrayList;
import java.util.List;


/**
 * 
 * @author vandejd1
 */
public class DelimitedYMDHMSFormatter implements IDelimitedTimeFormatter {
  private List<String> colNames;
  private String separator;

  public DelimitedYMDHMSFormatter(String separator) {
    this.separator = separator;
    colNames = new ArrayList<String>();
    colNames.add("Year");
    colNames.add("Month");
    colNames.add("Day");
    colNames.add("Hour");
    colNames.add("Minute");
    colNames.add("Second");
  }

  @Override
  public List<String> getComponentNames() {
    return colNames;
  }

  @Override
  public String getFormattedTime(UTCEpoch utc) {
    return String.format("%04d%s%02d%s%02d%s%02d%s%02d%s%6.3f", utc.getYear(), separator,
        utc.getMonth(), separator, utc.getDom(), separator, utc.getHour(), separator, utc.getMin(),
        separator, utc.getSec());
  }

}
