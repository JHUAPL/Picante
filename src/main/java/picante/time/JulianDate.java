package picante.time;

/**
 * Class representing a Julian Date (UTC).
 *
 * @author Hari.Nair@jhuapl.edu
 */
public class JulianDate implements Comparable<JulianDate> {

  private final double jd;

  @Override
  public int compareTo(JulianDate o) {
    return Double.compare(jd, o.jd);
  }

  @Override
  public String toString() {
    return "JD " + jd;
  }

  public JulianDate(double jd) {
    this.jd = jd;
  }

  public double getDate() {
    return jd;
  }

  /**
   * From section 12.92 of the Explanatory Supplement to the Astronomical Almanac.
   *
   * @param utc input UTC epoch
   * @return Equivalent Julian date
   */
  public static JulianDate fromUTCEpoch(UTCEpoch utc) {
    UTCEpoch noon = new UTCEpoch(utc.getYear(), utc.getDoy(), 12, 0, 0.);
    int Y = noon.getYear();
    int M = noon.getMonth();
    int D = noon.getDom();

    int jdFloor =
        (1461 * (Y + 4800 + (M - 14) / 12)) / 4
            + (367 * (M - 2 - 12 * ((M - 14) / 12))) / 12
            - (3 * ((Y + 4900 + (M - 14) / 12) / 100)) / 4
            + D
            - 32075;
    double jd = (utc.getHour() - 12) / 24. + utc.getMin() / 1440. + utc.getSec() / 86400. + jdFloor;

    return new JulianDate(jd);
  }

  /**
   * From section 12.92 of the Explanatory Supplement to the Astronomical Almanac.
   *
   * @return Equivalent UTCEpoch
   */
  public UTCEpoch toUTCEpoch() {

    int JD = (int) Math.round(jd);

    int L = JD + 68569;
    int N = 4 * L / 146097;
    L = L - (146097 * N + 3) / 4;
    int I = 4000 * (L + 1) / 1461001;
    L = L - 1461 * I / 4 + 31;
    int J = 80 * L / 2447;
    int D = L - 2447 * J / 80;
    L = J / 11;
    int M = J + 2 - 12 * L;
    int Y = 100 * (N - 49) + I + L;

    double fracDay = jd - JD;
    double fracHour = 24 * fracDay;
    double fracMin = 1440 * fracDay;
    int h = (int) (12 + fracHour);
    int m = (int) (60 * (fracHour - Math.floor(fracHour)));
    double s = 60 * (fracMin - Math.floor(fracMin));

    return new UTCEpoch(Y, M, D, h, m, s);
  }
}
