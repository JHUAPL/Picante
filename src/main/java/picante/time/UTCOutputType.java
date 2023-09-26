package picante.time;

public enum UTCOutputType {
  ET("Ephemeris Time", "Seconds", 1.0) {
    @Override
    public double getTime(double et) {
      return et;
    }
  },
  fractionalHourOfDay("Fractional Year", "Hours", 1.0 / (3600.0)) {
    @Override
    public double getTime(double et) {
      return et2FractionalHourOfDay(et);
    }


  },
  fractionalYear("Fractional Year", "Years", 1.0 / (86400.0 * 365.0)) {
    @Override
    public double getTime(double et) {
      return et2FractionalYear(et);
    }

  },
  fractionalDayOfYear("Day of Year", "Days", 1.0 / 86400.0) {
    @Override
    public double getTime(double et) {
      return et2FractionalDayOfYear(et);
    }
  };


  private final String label;
  private String units;
  private double minBinVal;

  UTCOutputType(String label, String units, double minBinVal) {
    this.label = label;
    this.units = units;
    this.minBinVal = minBinVal;
  }

  abstract public double getTime(double et);

  private static double getET(UTCEpoch utc) {
    return TimeAdapter.getInstance().convertToET(utc);
  }


  private static double getET(int year, int day, int hour, int min, double sec) {
    return TimeAdapter.getInstance().convertToET(new UTCEpoch(year, day, hour, min, sec));
  }

  private static TimeSystem<UTCEpoch> utcConverter = TSManager.getTimeSystem(TSManager.UTC);
  private static TimeSystem<Double> etConverter = TSManager.getTimeSystem(TSManager.ET);

  private static double et2FractionalHourOfDay(double et) {
    TSEpoch ts = etConverter.getTSEpoch(et);
    UTCEpoch utc = utcConverter.getTime(ts);
    double day = utc.getDoy();
    double fracDOY = et2FractionalDayOfYear(et);
    double fracOfDay = fracDOY - day;
    return fracOfDay * 24.0;
  }



  private static double et2FractionalDayOfYear(Double et) {
    TSEpoch ts = etConverter.getTSEpoch(et);
    UTCEpoch utc = utcConverter.getTime(ts);
    double day = utc.getDoy();
    double startOfDayET = getET(utc.getYear(), utc.getDoy(), 0, 0, 0.0);
    double t0ET = getET(utc);
    double frac = (t0ET - startOfDayET) / 86400.0;
    return day + frac;
  }

  private static double et2FractionalYear(double et) {
    TSEpoch ts = etConverter.getTSEpoch(et);
    UTCEpoch utc = utcConverter.getTime(ts);
    double yStart = getET(utc.getYear(), 0, 0, 0, 0);
    double dy = getET(utc.getYear() + 1, 0, 0, 0, 0) - yStart;
    return utc.getYear() + (et - yStart) / dy;
  }

  public String getLabel() {
    return this.label;
  }

  public String getUnits() {
    return this.units;
  }

  public double getMinBinVal() {
    return this.minBinVal;
  }

  public double[] convertTimeArray(double[] et) {
    double[] ret = new double[et.length];
    for (int i = 0; i < et.length; i++) {
      try {
        ret[i] = (this.getTime(et[i]));
      } catch (RuntimeException e) {
        // TODO: ask LArry why this was here:
        // int a=0;
        // I commented it out on 2010-10-15.
        // -JonV.
      }
    }
    return ret;
  }
}
