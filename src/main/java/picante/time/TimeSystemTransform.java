package picante.time;

import picante.time.TimeSystem.ParameterSpace;

/**
 * A convenience interface with static methods for simplifying a transform between two
 * {@link TimeSystem}s.
 * 
 * @author rodgedj1
 *
 * @param <T> the base time type of the 'To' time system (eg., UTC, or ET)
 * @param <F> the base time type of the 'From' time system (eg., UTC, or ET)
 * 
 */
@FunctionalInterface
public interface TimeSystemTransform<T, F> {

  /**
   * Returns the time T (associated with {@link TimeSystem}<code><</code>T<code>></code>) from the
   * input time F (associated with {@link TimeSystem}<code><</code>F<code>></code>).
   */
  public T from(F time);

  /**
   * Creates a {@link TimeSystemTransform} to {@link TimeSystem} 'toTimeSystem' from
   * {@link TimeSystem} 'fromTimeSystem'.
   */
  public static <T, F> TimeSystemTransform<T, F> create(TimeSystem<T> toTimeSystem,
      TimeSystem<F> fromTimeSystem) {
    return time -> toTimeSystem.getTime(fromTimeSystem.getTSEpoch(time));
  }

  // To TDB

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from Terrestial Dynamical Time (TDT) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> tdbFromTDT() {
    return tdbFromTDT(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from International Atomic Time (TAI, from the French name Temps Atomique International) using
   * default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> tdbFromTAI() {
    return tdbFromTAI(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from the time used by the Global Positioning System (GPS) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> tdbFromGPS() {
    return tdbFromGPS(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link UTCEpoch}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from Coordinated Universal Time (UTC) using default internal parameters.
   */
  public static TimeSystemTransform<Double, UTCEpoch> tdbFromUTC() {
    return tdbFromUTC(ParameterSpace.createDefault());
  }

  // To TDT

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Terrestial Dynamical Time (TDT) from Barycentric Dynamical Time (TDB, from the French
   * name Temps Dynamique Barycentrique) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> tdtFromTDB() {
    return tdtFromTDB(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Terrestial Dynamical Time (TDT) from International Atomic Time (TAI, from the French
   * name Temps Atomique International) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> tdtFromTAI() {
    return tdtFromTAI(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Terrestial Dynamical Time (TDT) from the time used by the Global Positioning System
   * (GPS) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> tdtFromGPS() {
    return tdtFromGPS(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link UTCEpoch}> for
   * obtaining Terrestial Dynamical Time (TDT) from Coordinated Universal Time (UTC) using default
   * internal parameters.
   */
  public static TimeSystemTransform<Double, UTCEpoch> tdtFromUTC() {
    return tdtFromUTC(ParameterSpace.createDefault());
  }

  // To TAI

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique) using
   * default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> taiFromTDB() {
    return taiFromTDB(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from Terrestial Dynamical Time (TDT) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> taiFromTDT() {
    return taiFromTDT(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from the time used by the Global Positioning System (GPS) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> taiFromGPS() {
    return taiFromGPS(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link UTCEpoch}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from Coordinated Universal Time (UTC) using default internal parameters.
   */
  public static TimeSystemTransform<Double, UTCEpoch> taiFromUTC() {
    return taiFromUTC(ParameterSpace.createDefault());
  }

  // To GPS

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from Barycentric Dynamical Time
   * (TDB, from the French name Temps Dynamique Barycentrique) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> gpsFromTDB() {
    return gpsFromTDB(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from Terrestial Dynamical Time
   * (TDT) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> gpsFromTDT() {
    return gpsFromTDT(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from International Atomic Time
   * (TAI, from the French name Temps Atomique International) using default internal parameters.
   */
  public static TimeSystemTransform<Double, Double> gpsFromTAI() {
    return gpsFromTAI(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from Coordinated Universal Time
   * (UTC) using default internal parameters.
   */
  public static TimeSystemTransform<Double, UTCEpoch> gpsFromUTC() {
    return gpsFromUTC(ParameterSpace.createDefault());
  }

  // To UTC

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from Barycentric Dynamical Time (TDB, from the
   * French name Temps Dynamique Barycentrique) using default internal parameters.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromTDB() {
    return utcFromTDB(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from Terrestial Dynamical Time (TDT) using default
   * internal parameters.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromTDT() {
    return utcFromTDT(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from International Atomic Time (TAI, from the French
   * name Temps Atomique International) using default internal parameters.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromTAI() {
    return utcFromTAI(ParameterSpace.createDefault());
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from the time used by the Global Positioning System
   * (GPS) using default internal parameters.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromGPS() {
    return utcFromGPS(ParameterSpace.createDefault());
  }

  // To TDB

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from Terrestial Dynamical Time (TDT).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> tdbFromTDT(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    return create(tdb, TimeSystem.tdt(parameterSpace, tdb));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from International Atomic Time (TAI, from the French name Temps Atomique International).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> tdbFromTAI(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(tdb, TimeSystem.tai(parameterSpace, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from the time used by the Global Positioning System (GPS).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> tdbFromGPS(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(tdb, TimeSystem.gps(parameterSpace, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link UTCEpoch}> for
   * obtaining Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique)
   * from Coordinated Universal Time (UTC).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, UTCEpoch> tdbFromUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    return create(tdb, TimeSystem.utc(parameterSpace, tdb));
  }

  // To TDT

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Terrestial Dynamical Time (TDT) from Barycentric Dynamical Time (TDB, from the French
   * name Temps Dynamique Barycentrique).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> tdtFromTDB(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    return create(TimeSystem.tdt(parameterSpace, tdb), tdb);
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Terrestial Dynamical Time (TDT) from International Atomic Time (TAI, from the French
   * name Temps Atomique International).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> tdtFromTAI(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(tdt, TimeSystem.tai(parameterSpace, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Terrestial Dynamical Time (TDT) from the time used by the Global Positioning System
   * (GPS).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> tdtFromGPS(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(tdt, TimeSystem.gps(parameterSpace, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link UTCEpoch}> for
   * obtaining Terrestial Dynamical Time (TDT) from Coordinated Universal Time (UTC).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, UTCEpoch> tdtFromUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(tdt, TimeSystem.utc(parameterSpace, tdb));
  }

  // To TAI

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from Barycentric Dynamical Time (TDB, from the French name Temps Dynamique Barycentrique).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> taiFromTDB(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.tai(parameterSpace, tdt), tdb);
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from Terrestial Dynamical Time (TDT).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> taiFromTDT(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.tai(parameterSpace, tdt), tdt);
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from the time used by the Global Positioning System (GPS).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> taiFromGPS(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.tai(parameterSpace, tdt), TimeSystem.gps(parameterSpace, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link UTCEpoch}> for
   * obtaining International Atomic Time (TAI, from the French name Temps Atomique International)
   * from Coordinated Universal Time (UTC).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, UTCEpoch> taiFromUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.tai(parameterSpace, tdt), TimeSystem.utc(parameterSpace, tdb));
  }

  // To GPS

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from Barycentric Dynamical Time
   * (TDB, from the French name Temps Dynamique Barycentrique).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> gpsFromTDB(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.gps(parameterSpace, tdt), tdb);
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from Terrestial Dynamical Time
   * (TDT).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> gpsFromTDT(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.gps(parameterSpace, tdt), tdt);
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from International Atomic Time
   * (TAI, from the French name Temps Atomique International).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, Double> gpsFromTAI(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.gps(parameterSpace, tdt), TimeSystem.tai(parameterSpace, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining the time used by the Global Positioning System (GPS) from Coordinated Universal Time
   * (UTC).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<Double, UTCEpoch> gpsFromUTC(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.gps(parameterSpace, tdt), TimeSystem.utc(parameterSpace, tdb));
  }

  // To UTC

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from Barycentric Dynamical Time (TDB, from the
   * French name Temps Dynamique Barycentrique).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromTDB(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    return create(TimeSystem.utc(parameterSpace, tdb), tdb);
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from Terrestial Dynamical Time (TDT).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromTDT(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.utc(parameterSpace, tdb), tdt);
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from International Atomic Time (TAI, from the French
   * name Temps Atomique International).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromTAI(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.utc(parameterSpace, tdb), TimeSystem.tai(parameterSpace, tdt));
  }

  /**
   * Returns an instance of {@link TimeSystemTransform}<{@link Double}, {@link Double}> for
   * obtaining Coordinated Universal Time (UTC) from the time used by the Global Positioning System
   * (GPS).
   * <p>
   * See {@link TimeSystem.ParameterSpace} for information on the input.
   */
  public static TimeSystemTransform<UTCEpoch, Double> utcFromGPS(ParameterSpace parameterSpace) {
    TDBTimeSystem tdb = (TDBTimeSystem) TimeSystem.tdb();
    TDTTimeSystem tdt = TimeSystem.tdt(parameterSpace, tdb);
    return create(TimeSystem.utc(parameterSpace, tdb), TimeSystem.gps(parameterSpace, tdt));
  }

}
