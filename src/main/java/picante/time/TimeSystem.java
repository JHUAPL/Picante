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

/**
 * Defines a time system which is used to get times to and from a {@link TSEpoch}
 * 
 * @author brownle1, rodgedj1
 * 
 * @param <T> the base time type of this time system (eg., UTC, or ET); the native way that time is
 *        represented in this time system;
 * 
 *        All double values referenced in this interface are elapsed time in the fundamental unit of
 *        the time system.
 * 
 */
public interface TimeSystem<T> {

  TSEpoch getTSEpoch(T time);

  T getTime(TSEpoch tsepoch);

  /**
   * elapsed seconds between two points in time specified in the native coordinate system
   */
  public double difference(T t0, T t1);

  /**
   * elapsed seconds between two points in time specified as TSEpoch times (i.e., something like a -
   * b)
   */
  public double difference(TSEpoch a, TSEpoch b);

  /**
   * add the given number of elapsed seconds to the native time and return the result as a native
   * time
   */
  public T add(T t0, double elapsedSeconds);

  /**
   * add the given number of elapsed seconds to the TSEpoch and return the result as a TSEpoch
   */
  public TSEpoch add(TSEpoch t0, double deltaT);

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for the Barycentric Dynamical
   * Time (TDB, from the French name Temps Dynamique Barycentrique) using default internal
   * parameters.
   * <p>
   * For precise tuning, use {@link TimeSystem#tdb(ParameterSpace)}.
   */
  public static TimeSystem<Double> tdb() {
    return new TDBTimeSystem();
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link TDBQuadTime}> for Barycentric Dynamical
   * Time (TDB, from the French name Temps Dynamique Barycentrique) in quadruple precision using
   * default internal parameters.
   * <p>
   * For precise tuning, use {@link TimeSystem#tdbQuad(ParameterSpace)}.
   */
  public static TimeSystem<TDBQuadTime> tdbQuad() {
    return new TDBQuadTimeSystem();
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for Terrestial Dynamical Time
   * (TDT) using default internal parameters.
   * <p>
   * For precise tuning, use {@link TimeSystem#tdt(ParameterSpace)}.
   */
  public static TimeSystem<Double> tdt() {
    return tdt(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for International Atomic Time
   * (TAI, from the French name Temps Atomique International) using default internal parameters.
   * <p>
   * For precise tuning, use {@link TimeSystem#tai(ParameterSpace)}.
   */
  public static TimeSystem<Double> tai() {
    return tai(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for the time used by the Global
   * Positioning System (GPS) using default internal parameters.
   * <p>
   * For precise tuning, use {@link TimeSystem#gps(ParameterSpace)}.
   */
  public static TimeSystem<Double> gps() {
    return gps(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link UTCEpoch}> for Coordinated Universal Time
   * (UTC) using default internal parameters.
   * <p>
   * For precise tuning, use {@link TimeSystem#utc(ParameterSpace)}.
   */
  public static TimeSystem<UTCEpoch> utc() {
    return utc(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for the Barycentric Dynamical
   * Time (TDB, from the French name Temps Dynamique Barycentrique).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for more information.
   */
  public static TimeSystem<Double> tdb(ParameterSpace parameterSpace) {
    @SuppressWarnings("unused")
    ParameterSpace dum = parameterSpace; // suppressing "unused" without API-visible annotation
    return new TDBTimeSystem();
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link TDBQuadTime}> for Barycentric Dynamical
   * Time (TDB, from the French name Temps Dynamique Barycentrique) in quadruple precision.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for more information.
   */
  public static TimeSystem<TDBQuadTime> tdbQuad(ParameterSpace parameterSpace) {
    @SuppressWarnings("unused")
    ParameterSpace dum = parameterSpace; // suppressing "unused" without API-visible annotation
    return new TDBQuadTimeSystem();
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for Terrestial Dynamical Time
   * (TDT).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for more information.
   */
  public static TimeSystem<Double> tdt(ParameterSpace parameterSpace) {
    return tdt(parameterSpace, (TDBTimeSystem) tdb(parameterSpace));
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for International Atomic Time
   * (TAI, from the French name Temps Atomique International).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for more information.
   */
  public static TimeSystem<Double> tai(ParameterSpace parameterSpace) {
    return tai(parameterSpace, tdt(parameterSpace, (TDBTimeSystem) tdb(parameterSpace)));
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link Double}> for the time used by the Global
   * Positioning System (GPS).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for more information.
   */
  public static TimeSystem<Double> gps(ParameterSpace parameterSpace) {
    return gps(parameterSpace, tdt(parameterSpace, (TDBTimeSystem) tdb(parameterSpace)));
  }

  /**
   * Returns an instance of the {@link TimeSystem}<{@link UTCEpoch}> for Coordinated Universal Time
   * (UTC).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for more information.
   */
  public static TimeSystem<UTCEpoch> utc(ParameterSpace parameterSpace) {
    return utc(parameterSpace, (TDBTimeSystem) tdb(parameterSpace));
  }

  /**
   * Returns an instance of the {@link TDTTimeSystem} for the Terrestial Dynamical Time (TDT).
   * <p>
   * Java will <i>show</i> the method as available, but because {@link TDBTimeSystem} and
   * {@link TDTTimeSystem} are package-private, the method can not be used at the API level.
   * 
   */
  public static TDTTimeSystem tdt(ParameterSpace parameterSpace, TDBTimeSystem tdb) {
    return new TDTTimeSystem(parameterSpace.k, parameterSpace.eb, parameterSpace.m0,
        parameterSpace.m1, tdb);
  }

  /**
   * Returns an instance of the {@link TAITimeSystem} for the International Atomic Time (TAI, from
   * the French name Temps Atomique International).
   * <p>
   * Java will <i>show</i> the method as available, but because {@link TDTTimeSystem} and
   * {@link TAITimeSystem} are package-private, the method can not be used at the API level.
   */
  public static TAITimeSystem tai(ParameterSpace parameterSpace, TDTTimeSystem tdt) {
    return new TAITimeSystem(parameterSpace.delta_t_a, tdt);
  }

  /**
   * Returns an instance of the {@link GPSTimeSystem} for the time used by the Global Positioning
   * System (GPS).
   * <p>
   * Java will <i>show</i> the method as available, but because {@link TDTTimeSystem} and
   * {@link GPSTimeSystem} are package-private, the method can not be used at the API level.
   */
  public static GPSTimeSystem gps(ParameterSpace parameterSpace, TDTTimeSystem tdt) {
    return new GPSTimeSystem(parameterSpace.gps_tai_dt, parameterSpace.delta_t_a, tdt);
  }

  /**
   * Returns an instance of the {@link UTCTimeSystem} for Coordinated Universal Time (UTC).
   * <p>
   * Java will <i>show</i> the method as available, but because {@link TDBTimeSystem} and
   * {@link UTCTimeSystem} are package-private, the method can not be used at the API level.
   */
  public static UTCTimeSystem utc(ParameterSpace parameterSpace, TDBTimeSystem tdb) {
    return new UTCTimeSystem(
        new UTCFactory(parameterSpace.dayTable, parameterSpace.taiTable, parameterSpace.delta_t_a,
            parameterSpace.eb, parameterSpace.k, parameterSpace.m0, parameterSpace.m1),
        tdb);
  }

  /**
   * Immutable class containing all parameters necessary for creating any {@link TimeSystem}.
   * <p>
   * ParameterSpace is a set of parameters whose internal data structure can not be used at the API
   * level; it can only be created using {@link ParameterSpace#createDefault()} or a
   * {@link ParameterSpace.Builder} (available via {@link ParameterSpace#builder()}).
   * 
   * @author rodgedj1
   *
   */
  public static class ParameterSpace {

    /**
     * Creates a {@link TimeSystem#ParameterSpace} using internal default parameters suitable for
     * creating any {@link TimeSystem} characterized by this system.
     */
    public static final ParameterSpace createDefault() {
      return builder().build();
    }

    /**
     * Creates a {@link TimeSystem.ParameterSpace#Builder} initially loaded with internal default
     * parameters suitable for creating any {@link TimeSystem} characterized by this system.
     */
    public static final Builder builder() {
      return new Builder();
    }

    /**
     * Builder used to create a {@link ParameterSpace} suitable for creating any {@link TimeSystem}.
     * 
     * @author rodgedj1
     *
     */
    public static final class Builder
        implements picante.designpatterns.Builder<ParameterSpace, RuntimeException> {

      @Override
      public ParameterSpace build() throws RuntimeException {
        buildLeapsecondsTable();
        return new ParameterSpace(gps_tai_dt, delta_t_a, k, eb, m0, m1, leapseconds, taiTable,
            dayTable);
      }

      /**
       * If you're using this, you know what it's for...
       */
      public Builder withGPS_TAI_DT(double gps_tai_dt) {
        this.gps_tai_dt = gps_tai_dt;
        return this;
      }

      /**
       * If you're using this, you know what it's for...
       */
      public Builder withDeltaT_A(double delta_t_a) {
        this.delta_t_a = delta_t_a;
        return this;
      }

      /**
       * If you're using this, you know what it's for...
       */
      public Builder withK(double k) {
        this.k = k;
        return this;
      }

      /**
       * If you're using this, you know what it's for...
       */
      public Builder withEB(double eb) {
        this.eb = eb;
        return this;
      }

      /**
       * If you're using this, you know what it's for...
       */
      public Builder withM0(double m0) {
        this.m0 = m0;
        return this;
      }

      /**
       * If you're using this, you know what it's for...
       */
      public Builder withM1(double m1) {
        this.m1 = m1;
        return this;
      }

      /**
       * If you're using this, you know what it's for...
       */
      public Builder withLeapseconds(NavigableSet<LeapsecondEntry> leapseconds) {
        this.leapseconds = ImmutableSortedSet.copyOf(checkNotNull(leapseconds));
        return this;
      }

      private Builder() {
        populateDefaultsFromInternalPropertiesFile();
      }

      private double[] taiTable;
      private int[] dayTable;
      private double gps_tai_dt;
      private double delta_t_a;
      private double k;
      private double eb;
      private double m0;
      private double m1;
      private NavigableSet<LeapsecondEntry> leapseconds;

      /**
       * The number of seconds in one half of a day on a formal, no leapseconds calendar.
       */
      private static final double HALF_DAY = SECONDS_PER_DAY / 2.0;

      /**
       * The number of days, on the Gregorian calendar that have ellapsed between Jan 1, 1 A.D. and
       * Jan 1, 2000.
       */
      private static final int DAY_NUMBER_OF_J2000 = 730119;

      private final void populateDefaultsFromInternalPropertiesFile() {

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
        delta_t_a =
            Double.parseDouble(properties.getProperty("picante.time.deltet.delta_t_a"));

        String[] fields = properties.getProperty("picante.time.deltet.m").split(",");
        m0 = Double.parseDouble(fields[0]);
        m1 = Double.parseDouble(fields[1]);

        fields = properties.getProperty("picante.time.deltet.delta_at").split(",");

        ImmutableSortedSet.Builder<LeapsecondEntry> lsk = ImmutableSortedSet.naturalOrder();

        for (int i = 0; i < fields.length; i += 2) {
          lsk.add(new LeapsecondEntry(Double.parseDouble(fields[i]),
              Double.parseDouble(fields[i + 1])));
        }

        leapseconds = lsk.build();
      }

      /**
       * Update the internal leapsecond tables from the cached reference to the kernel pool used to
       * create this kernel.
       * 
       * @throws NullPointerException if the contents of the kernel pool are not as this method
       *         expects.
       */
      private final void buildLeapsecondsTable() {

        /*
         * We've already hit the pool to determine we have new values to load up. Just toss our
         * prior arrays and initialize new ones.
         */
        taiTable = new double[2 * leapseconds.size()];
        dayTable = new int[2 * leapseconds.size()];

        /*
         * At this point, we have to assume that we have a properly formatted kernel pool with
         * properly specified leapseconds. As a point of fact, no one should be messing around with
         * the contents of the leapseconds table, unless they really know what they're doing
         * anyways.
         * 
         * Note: the way this particular class functions, prior to the first specified set of
         * leapseconds (hopefully 1972) it assumes the change between TAI and UTC is a constant
         * which is precisely one less than the DUT specified by the first entry in the table.
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


    }

    private ParameterSpace(double gps_tai_dt, double delta_t_a, double k, double eb, double m0,
        double m1, NavigableSet<LeapsecondEntry> leapseconds, double[] taiTable, int[] dayTable) {
      this.gps_tai_dt = gps_tai_dt;
      this.delta_t_a = delta_t_a;
      this.k = k;
      this.eb = eb;
      this.m0 = m0;
      this.m1 = m1;
      this.leapseconds = leapseconds;
      this.taiTable = taiTable;
      this.dayTable = dayTable;
    }

    private final double gps_tai_dt;
    private final double delta_t_a;
    private final double k;
    private final double eb;
    private final double m0;
    private final double m1;
    private final NavigableSet<LeapsecondEntry> leapseconds;
    private final double[] taiTable;
    private final int[] dayTable;

    @VisibleForTesting
    private final double getGps_tai_dt() {
      return gps_tai_dt;
    }

    @VisibleForTesting
    private final double getDelta_t_a() {
      return delta_t_a;
    }

    @VisibleForTesting
    private final double getK() {
      return k;
    }

    @VisibleForTesting
    private final double getEb() {
      return eb;
    }

    @VisibleForTesting
    private final double getM0() {
      return m0;
    }

    @VisibleForTesting
    private final double getM1() {
      return m1;
    }

    @VisibleForTesting
    private final NavigableSet<LeapsecondEntry> getLeapseconds() {
      return leapseconds;
    }

    @VisibleForTesting
    private final double[] getTaiTable() {
      return taiTable;
    }

    @VisibleForTesting
    private final int[] getDayTable() {
      return dayTable;
    }



  }

}
