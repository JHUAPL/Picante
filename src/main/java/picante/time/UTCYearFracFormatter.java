/**
 * Filename: UTCYearFracFormatter.java Author : vandejd1 Created : Dec 15, 2008
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
public class UTCYearFracFormatter implements IDelimitedTimeFormatter {

  private List<String> yrFracName;

  public UTCYearFracFormatter() {
    yrFracName = new ArrayList<String>();
    yrFracName.add("fractional_year_in_UTC");
  }

  @Override
  public String getFormattedTime(UTCEpoch utc) {

    double yrFrac = getYearFraction(utc);
    return String.format("%15.10f", yrFrac);
  }

  public double getYearFraction(UTCEpoch utc) {
    TimeAdapter ta = TimeAdapter.getInstance();
    // see what fraction of the year this is:
    double etAtYrStart = ta.convertToET(new UTCEpoch(utc.getYear(), 1, 0, 0, 0));
    double etAtYearEnd = ta.convertToET(new UTCEpoch(utc.getYear() + 1, 1, 0, 0, 0));
    double etNow = ta.convertToET(utc);
    double yrFrac = utc.getYear() + (etNow - etAtYrStart) / (etAtYearEnd - etAtYrStart);

    return yrFrac;
  }

  @Override
  public List<String> getComponentNames() {
    return yrFracName;
  }

}
