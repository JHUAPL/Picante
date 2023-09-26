package picante.time;

import static com.google.common.base.Preconditions.checkNotNull;
import static picante.units.FundamentalPhysicalConstants.SECONDS_PER_DAY;
import java.io.IOException;
import java.util.NavigableSet;
import java.util.Properties;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.io.Resources;
import picante.exceptions.BugException;

public class TimeSystems {

  public static class Builder
      implements picante.designpatterns.Builder<TimeSystems, RuntimeException> {

    /**
     * The number of seconds in one half of a day on a formal, no leapseconds calendar.
     */
    private static final double HALF_DAY = SECONDS_PER_DAY / 2.0;

    /**
     * The number of days, on the Gregorian calendar that have ellapsed between Jan 1, 1 A.D. and
     * Jan 1, 2000.
     */
    private static final int DAY_NUMBER_OF_J2000 = 730119;

    private Builder() {
      populateDefaultsFromInternalPropertiesFile();
    }


    public void populateDefaultsFromInternalPropertiesFile() {

      Properties properties = new Properties();
      try {
        properties
            .load(Resources.getResource("picante/time/parameters.properties").openStream());
      } catch (IOException e) {
        throw new BugException("picante/time/parameters.properties file on the "
            + "classpath is missing or incomplete");
      }

      k = Double.parseDouble(properties.getProperty("picante.time.deltet.k"));
      eb = Double.parseDouble(properties.getProperty("picante.time.deltet.eb"));
      gps_tai_dt = Double.parseDouble(properties.getProperty("picante.time.gps_tai_dt"));
      delta_t_a = Double.parseDouble(properties.getProperty("picante.time.deltet.delta_t_a"));

      String[] fields = properties.getProperty("picante.time.deltet.m").split(",");
      m0 = Double.parseDouble(fields[0]);
      m1 = Double.parseDouble(fields[1]);

      fields = properties.getProperty("picante.time.deltet.delta_at").split(",");

      ImmutableSortedSet.Builder<LeapsecondEntry> lsk = ImmutableSortedSet.naturalOrder();

      for (int i = 0; i < fields.length; i += 2) {
        lsk.add(
            new LeapsecondEntry(Double.parseDouble(fields[i]), Double.parseDouble(fields[i + 1])));
      }

      leapseconds = lsk.build();
    }

    @Override
    public TimeSystems build() {

      buildLeapsecondsTable();

      TDBTimeSystem tdb = new TDBTimeSystem();
      TDBQuadTimeSystem tdbQuad = new TDBQuadTimeSystem();
      TDTTimeSystem tdt = new TDTTimeSystem(k, eb, m0, m1, tdb);
      TAITimeSystem tai = new TAITimeSystem(delta_t_a, tdt);
      GPSTimeSystem gps = new GPSTimeSystem(gps_tai_dt, delta_t_a, tdt);
      UTCFactory utcFactory = new UTCFactory(dayTable, taiTable, delta_t_a, eb, k, m0, m1);
      UTCTimeSystem utc = new UTCTimeSystem(utcFactory, tdb);

      return new TimeSystems(tdb, tdbQuad, tai, tdt, gps, utc);
    }

    /**
     * Update the internal leapsecond tables from the cached reference to the kernel pool used to
     * create this kernel.
     * 
     * @throws NullPointerException if the contents of the kernel pool are not as this method
     *         expects.
     */
    void buildLeapsecondsTable() {

      /*
       * We've already hit the pool to determine we have new values to load up. Just toss our prior
       * arrays and initialize new ones.
       */
      taiTable = new double[2 * leapseconds.size()];
      dayTable = new int[2 * leapseconds.size()];

      /*
       * At this point, we have to assume that we have a properly formatted kernel pool with
       * properly specified leapseconds. As a point of fact, no one should be messing around with
       * the contents of the leapseconds table, unless they really know what they're doing anyways.
       * 
       * Note: the way this particular class functions, prior to the first specified set of
       * leapseconds (hopefully 1972) it assumes the change between TAI and UTC is a constant which
       * is precisely one less than the DUT specified by the first entry in the table.
       * 
       * TODO: The definition of leapseconds prior to 1972 here is not exactly correct. We could
       * consider replicating the USNO definition of civil time or the other half a dozen systems
       * that might have been around back then.
       */
      double lastDut = leapseconds.first().getDut() - 1.0;

      int i = 0;

      for (LeapsecondEntry entry : leapseconds) {

        double dut = entry.getDut();
        double formal = entry.getFormalEpoch();

        taiTable[i] = formal - SECONDS_PER_DAY + lastDut;
        taiTable[i + 1] = formal + dut;

        /*
         * Populate dayTable. Since formal specifies a formal time in seconds past the J2000 epoch
         * which refers to noon, add a half of day worth of seconds to formal. This gives us the
         * formal time associated with the start of the day when the DUT change takes effect.
         */
        int dayNumber = (int) ((formal + HALF_DAY) / SECONDS_PER_DAY) + DAY_NUMBER_OF_J2000;

        dayTable[i] = dayNumber - 1;
        dayTable[i + 1] = dayNumber;

        /*
         * Update the value of lastDut to contain the one we just processed.
         */
        lastDut = dut;

        /*
         * Increment the pointer into the table arrays by 2.
         */
        i += 2;
      }
    }



    private double gps_tai_dt;

    public Builder withGPS_TAI_DT(double gps_tai_dt) {
      this.gps_tai_dt = gps_tai_dt;
      return this;
    }

    private double delta_t_a;

    public Builder withDeltaT_A(double delta_t_a) {
      this.delta_t_a = delta_t_a;
      return this;
    }

    private double k;

    public Builder withK(double k) {
      this.k = k;
      return this;
    }

    private double eb;

    public Builder withEB(double eb) {
      this.eb = eb;
      return this;
    }

    private double m0;

    public Builder withM0(double m0) {
      this.m0 = m0;
      return this;
    }

    private double m1;

    public Builder withM1(double m1) {
      this.m1 = m1;
      return this;
    }

    private NavigableSet<LeapsecondEntry> leapseconds;
    private double[] taiTable;
    private int[] dayTable;

    @VisibleForTesting
    double getGps_tai_dt() {
      return gps_tai_dt;
    }

    @VisibleForTesting
    double getDelta_t_a() {
      return delta_t_a;
    }

    @VisibleForTesting
    double getK() {
      return k;
    }

    @VisibleForTesting
    double getEb() {
      return eb;
    }

    @VisibleForTesting
    double getM0() {
      return m0;
    }

    @VisibleForTesting
    double getM1() {
      return m1;
    }

    @VisibleForTesting
    NavigableSet<LeapsecondEntry> getLeapseconds() {
      return leapseconds;
    }

    @VisibleForTesting
    double[] getTaiTable() {
      return taiTable;
    }

    @VisibleForTesting
    int[] getDayTable() {
      return dayTable;
    }


    public Builder withLeapseconds(NavigableSet<LeapsecondEntry> leapseconds) {
      this.leapseconds = ImmutableSortedSet.copyOf(checkNotNull(leapseconds));
      return this;
    }
  }

  public static Builder builder() {
    return new Builder();
  }

  private TimeSystems(TDBTimeSystem tdb, TDBQuadTimeSystem tdbQuad, TAITimeSystem tai,
      TDTTimeSystem tdt, GPSTimeSystem gps, UTCTimeSystem utc) {
    this.tdb = tdb;
    this.tdbQuad = tdbQuad;
    this.tai = tai;
    this.tdt = tdt;
    this.gps = gps;
    this.utc = utc;
  }

  public static TimeSystems createUsingInternalConstants() {
    return builder().build();
  }

  private final TDBTimeSystem tdb;
  private final TDBQuadTimeSystem tdbQuad;
  private final TAITimeSystem tai;
  private final TDTTimeSystem tdt;
  private final GPSTimeSystem gps;
  private final UTCTimeSystem utc;

  public TimeSystem<Double> getTDB() {
    return tdb;
  }

  public TimeSystem<TDBQuadTime> getTDBQuadTime() {
    return tdbQuad;
  }

  public TimeSystem<Double> getTAI() {
    return tai;
  }

  public TimeSystem<Double> getTDT() {
    return tdt;
  }

  public TimeSystem<Double> getGPS() {
    return gps;
  }

  public TimeSystem<UTCEpoch> getUTC() {
    return utc;
  }

  public static void main(String[] args) {
    Builder b = builder();
    TimeSystem<UTCEpoch> utc = b.build().getUTC();
    TimeSystem<Double> tdb = b.build().getTDB();
    UTCEpoch epoch = new UTCEpoch(2020, 001, 12, 00, 0);

    TSEpoch e = utc.getTSEpoch(epoch);
    double et = tdb.getTime(e);

    System.out.println(et);


  }

}
