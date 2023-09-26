package picante.time;

import picante.time.TimeSystem.ParameterSpace;

/**
 * A convenience interface with static methods for consolidating the pair of
 * {@link TimeSystemTransform}s between two {@link TimeSystem}s.
 * <p>
 * <code>
 * forward transform : T from F
 * <p>
 * inverse transform : F from T
 * </code>
 * <p>
 * If you would like use one of the static creation methods but would prefer the opposite
 * convention, create the instance and use the {@link #reverse()} method.
 * 
 * @author rodgedj1
 *
 * @param <T> the base time type of the 'To' time system (eg., UTC, or ET)
 * @param <F> the base time type of the 'From' time system (eg., UTC, or ET)
 * 
 */
public interface TimeSystemConverter<T, F> {

  /**
   * Returns the time T (associated with {@link TimeSystem}<code><</code>T<code>></code>) from the
   * input time F (associated with {@link TimeSystem}<code><</code>F<code>></code>).
   */
  public T forward(F time);

  /**
   * Returns the time F (associated with {@link TimeSystem}<code><</code>F<code>></code>) from the
   * input time T (associated with {@link TimeSystem}<code><</code>T<code>></code>).
   */
  public F inverse(T time);

  /**
   * Returns this {@link TimeSystemConverter}<<code>T</code>, <code>F</code>> with the transforms
   * reversed so that <code>forward &#10230 inverse</code>, <code>inverse &#10230 forward</code>,
   * and {@link TimeSystemConverter}<<code>F</code>, <code>T</code>> is returned.
   */
  public default TimeSystemConverter<F, T> reverse() {
    return create(t -> inverse(t), t -> forward(t));
  }

  /**
   * Creates a {@link TimeSystemConverter} with the transform pair consisting of the
   * {@link TimeSystemTransform} 'forwardTransform' and the {@link TimeSystemTransform}
   * 'inverseTransform'.
   */
  public static <T, F> TimeSystemConverter<T, F> create(TimeSystemTransform<T, F> forwardTransform,
      TimeSystemTransform<F, T> inverseTransform) {
    return new TimeSystemConverter<T, F>() {

      @Override
      public T forward(F time) {
        return forwardTransform.from(time);
      }

      @Override
      public F inverse(T time) {
        return inverseTransform.from(time);
      }

    };
  }

  // TDB

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and Terrestial Dynamical Time (TDT) using default internal parameters.
   * <p>
   * <code>
   * forward : TDB from TDT
   * <p>
   * inverse : TDT from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, Double> tdbAndTDT() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tdb, tdt), TimeSystemTransform.create(tdt, tdb));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and International Atomic Time (TAI, from the French name Temps Atomique
   * International) using default internal parameters.
   * <p>
   * <code>
   * forward : TDB from TAI
   * <p>
   * inverse : TAI from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, Double> tdbAndTAI() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, TimeSystem.tdt(parameterSpace, tdb));
    return create(TimeSystemTransform.create(tdb, tai), TimeSystemTransform.create(tai, tdb));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and the time used by the Global Positioning System (GPS) using default internal
   * parameters.
   * <p>
   * <code>
   * forward : TDB from GPS
   * <p>
   * inverse : GPS from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, Double> tdbAndGPS() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, TimeSystem.tdt(parameterSpace, tdb));
    return create(TimeSystemTransform.create(tdb, gps), TimeSystemTransform.create(gps, tdb));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and Coordinated Universal Time (UTC) using default internal parameters.
   * <p>
   * <code>
   * forward : TDB from UTC
   * <p>
   * inverse : UTC from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, UTCEpoch> tdbAndUTC() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tdb, utc), TimeSystemTransform.create(utc, tdb));
  }

  // TDT

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Terrestial Dynamical Time (TDT) and International Atomic Time (TAI, from
   * the French name Temps Atomique International) using default internal parameters.
   * <p>
   * <code>
   * forward : TDT from TAI
   * <p>
   * inverse : TAI from TDT
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, Double> tdtAndTAI() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, (TDBTimeSystem) TimeSystem.tdb());
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, tdt);
    return create(TimeSystemTransform.create(tdt, tai), TimeSystemTransform.create(tai, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Terrestial Dynamical Time (TDT) and the time used by the Global
   * Positioning System (GPS) using default internal parameters.
   * <p>
   * <code>
   * forward : TDT from GPS
   * <p>
   * inverse : GPS from TDT
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, Double> tdtAndGPS() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, (TDBTimeSystem) TimeSystem.tdb());
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, tdt);
    return create(TimeSystemTransform.create(tdt, gps), TimeSystemTransform.create(gps, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between Terrestial Dynamical Time (TDT) and Coordinated Universal Time (UTC) using
   * default internal parameters.
   * <p>
   * <code>
   * forward : TDT from UTC
   * <p>
   * inverse : UTC from TDT
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, UTCEpoch> tdtAndUTC() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tdt, utc), TimeSystemTransform.create(utc, tdt));
  }

  // TAI

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between International Atomic Time (TAI, from the French name Temps Atomique
   * International) and the time used by the Global Positioning System (GPS).
   * <p>
   * <code>
   * forward : TAI from GPS
   * <p>
   * inverse : GPS from TAI
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, Double> taiAndGPS() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, (TDBTimeSystem) TimeSystem.tdb());
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, tdt);
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, tdt);
    return create(TimeSystemTransform.create(tai, gps), TimeSystemTransform.create(gps, tai));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between International Atomic Time (TAI, from the French name Temps Atomique
   * International) and Coordinated Universal Time (UTC).
   * <p>
   * <code>
   * forward : TAI from UTC
   * <p>
   * inverse : UTC from TAI
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, UTCEpoch> taiAndUTC() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, tdt);
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tai, utc), TimeSystemTransform.create(utc, tai));
  }

  // GPS

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between the time used by the Global Positioning System (GPS) and Coordinated
   * Universal Time (UTC).
   * <p>
   * <code>
   * forward : GPS from UTC
   * <p>
   * inverse : UTC from GPS
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   */
  public static TimeSystemConverter<Double, UTCEpoch> gpsAndUTC() {
    ParameterSpace parameterSpace = ParameterSpace.createDefault();
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, tdt);
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(gps, utc), TimeSystemTransform.create(utc, gps));
  }

  // TDB

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and Terrestial Dynamical Time (TDT).
   * <p>
   * <code>
   * forward : TDB from TDT
   * <p>
   * inverse : TDT from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, Double> tdbAndTDT(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tdb, tdt), TimeSystemTransform.create(tdt, tdb));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and International Atomic Time (TAI, from the French name Temps Atomique
   * International).
   * <p>
   * <code>
   * forward : TDB from TAI
   * <p>
   * inverse : TAI from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, Double> tdbAndTAI(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, TimeSystem.tdt(parameterSpace, tdb));
    return create(TimeSystemTransform.create(tdb, tai), TimeSystemTransform.create(tai, tdb));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and the time used by the Global Positioning System (GPS).
   * <p>
   * <code>
   * forward : TDB from GPS
   * <p>
   * inverse : GPS from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, Double> tdbAndGPS(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, TimeSystem.tdt(parameterSpace, tdb));
    return create(TimeSystemTransform.create(tdb, gps), TimeSystemTransform.create(gps, tdb));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between Barycentric Dynamical Time (TDB, from the French name Temps Dynamique
   * Barycentrique) and Coordinated Universal Time (UTC).
   * <p>
   * <code>
   * forward : TDB from UTC
   * <p>
   * inverse : UTC from TDB
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, UTCEpoch> tdbAndUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tdb, utc), TimeSystemTransform.create(utc, tdb));
  }

  // TDT

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Terrestial Dynamical Time (TDT) and International Atomic Time (TAI, from
   * the French name Temps Atomique International).
   * <p>
   * <code>
   * forward : TDT from TAI
   * <p>
   * inverse : TAI from TDT
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, Double> tdtAndTAI(ParameterSpace parameterSpace) {
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, (TDBTimeSystem) TimeSystem.tdb());
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, tdt);
    return create(TimeSystemTransform.create(tdt, tai), TimeSystemTransform.create(tai, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between Terrestial Dynamical Time (TDT) and the time used by the Global
   * Positioning System (GPS).
   * <p>
   * <code>
   * forward : TDT from GPS
   * <p>
   * inverse : GPS from TDT
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, Double> tdtAndGPS(ParameterSpace parameterSpace) {
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, (TDBTimeSystem) TimeSystem.tdb());
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, tdt);
    return create(TimeSystemTransform.create(tdt, gps), TimeSystemTransform.create(gps, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between Terrestial Dynamical Time (TDT) and Coordinated Universal Time (UTC).
   * <p>
   * <code>
   * forward : TDT from UTC
   * <p>
   * inverse : UTC from TDT
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, UTCEpoch> tdtAndUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tdt, utc), TimeSystemTransform.create(utc, tdt));
  }

  // TAI

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link Double}> for
   * transforming between International Atomic Time (TAI, from the French name Temps Atomique
   * International) and the time used by the Global Positioning System (GPS).
   * <p>
   * <code>
   * forward : TAI from GPS
   * <p>
   * inverse : GPS from TAI
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, Double> taiAndGPS(ParameterSpace parameterSpace) {
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, (TDBTimeSystem) TimeSystem.tdb());
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, tdt);
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, tdt);
    return create(TimeSystemTransform.create(tai, gps), TimeSystemTransform.create(gps, tai));
  }

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between International Atomic Time (TAI, from the French name Temps Atomique
   * International) and Coordinated Universal Time (UTC).
   * <p>
   * <code>
   * forward : TAI from UTC
   * <p>
   * inverse : UTC from TAI
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, UTCEpoch> taiAndUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    TAITimeSystem tai = TimeSystem.tai(parameterSpace, tdt);
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(tai, utc), TimeSystemTransform.create(utc, tai));
  }

  // GPS

  /**
   * Returns an instance of {@link TimeSystemConverter}<{@link Double}, {@link UTCEpoch}> for
   * transforming between the time used by the Global Positioning System (GPS) and Coordinated
   * Universal Time (UTC).
   * <p>
   * <code>
   * forward : GPS from UTC
   * <p>
   * inverse : UTC from GPS
   * </code>
   * <p>
   * If you would prefer the opposite convention to what is returned here, create the instance and
   * use the {@link #reverse()} method.
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemConverter<Double, UTCEpoch> gpsAndUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    GPSTimeSystem gps = TimeSystem.gps(parameterSpace, tdt);
    UTCTimeSystem utc = TimeSystem.utc(parameterSpace, tdb);
    return create(TimeSystemTransform.create(gps, utc), TimeSystemTransform.create(utc, gps));
  }

}
