/**
 * Filename: TimeAdapter.java Author : vandejd1 Created : Feb 15, 2010
 *
 * Copyright (C) 2008 The Johns Hopkins University Applied Physics Laboratory (JHU/APL) All rights
 * reserved
 */
package picante.time;

import java.time.LocalDateTime;
import picante.exceptions.PicanteRuntimeException;
import picante.math.intervals.UnwritableInterval;
import picante.spice.kernel.tk.lsk.LSK;

/**
 * a mechanism to handle common time conversions;
 *
 * it serves as a convenience wrapper around the {@link TSManager} and it is designed as a singleton
 * -- an application-wide and also a library-wide place to deal with different time systems and time
 * conversions;
 *
 * @author vandejd1
 */
public class TimeAdapter {

  private static TimeAdapter instance;

  /**
   * Get a singleton instance of the adapter. if its not created already, then it is created with
   * default implementations of UTC, ET, and Hi-Precision ET. If you initialized this singleton
   * yourself with your own time systems (yes, you can override the default implementations if you
   * want to, you crazy fool!), you get the time adapter based on a TSManager with those custom
   * systems.
   *
   * @return
   */
  public static TimeAdapter getInstance() {
    if (instance == null) {
      instance = new TimeAdapter();
    }
    return instance;
  }

  /**
   * use this method to prime a time adapter with the time systems of your own making; you have to
   * call this BEFORE asking for an instance of the singleton
   *
   * @param utcTimeSystem
   * @param etTimeSystem
   * @param hiPrecisionET
   */
  public static void initialize(TSManager tsManager) {
    if (instance != null) {
      throw new PicanteRuntimeException(
          "Error - attempt to initialize an already initialized TimeAdapter.");
    }
    instance = new TimeAdapter(tsManager);
  }

  /**
   * Initialize from an {@link LSK} object. This allows using a custom leap second kernel. This will
   * return a new TimeAdapter, not the singleton object. If you want to initialize the singleton
   * with this LSK, use
   * 
   * <pre>
   * TimeAdapter.initialize(TSManager.createStandardManager(lsk));
   * </pre>
   * 
   * @param lsk
   */
  public static TimeAdapter initialize(LSK lsk) {
    return new TimeAdapter(TSManager.createStandardManager(lsk));
  }

  private final TimeSystem<UTCEpoch> utcTimeSys;
  private final TimeSystem<Double> etTimeSys;
  private final TimeSystem<Double> taiTimeSys;
  private final TimeSystem<Double> tdtTimeSys;
  protected final TSManager tsManager;

  /**
   * this is protected to allow subclasses to extend the functionality by adding convenience methods
   * for other time systems.
   */
  protected TimeAdapter(TSManager customTsManager) {
    this.tsManager = customTsManager;
    utcTimeSys = tsManager.getKnownTimeSystem(TSManager.UTC);
    etTimeSys = tsManager.getKnownTimeSystem(TSManager.ET);
    taiTimeSys = tsManager.getKnownTimeSystem(TSManager.TAI);
    tdtTimeSys = tsManager.getKnownTimeSystem(TSManager.TDT);
  }

  /**
   * this is protected to allow subclasses to be able to instantiate a default instance;
   *
   * since these classes are package private, there is no other way to instantiate them
   */
  protected TimeAdapter() {
    TimeSystems timeSystems = TimeSystems.createUsingInternalConstants();
    tsManager = TSManager.createEmptyManager();
    tsManager.addTimeSystem(TSManager.UTC, timeSystems.getUTC());
    tsManager.addTimeSystem(TSManager.ET, timeSystems.getTDB());
    tsManager.addTimeSystem(TSManager.ETHiPrecision, timeSystems.getTDBQuadTime());
    tsManager.addTimeSystem(TSManager.TAI, timeSystems.getTAI());
    tsManager.addTimeSystem(TSManager.TDT, timeSystems.getTDT());

    utcTimeSys = tsManager.getKnownTimeSystem(TSManager.UTC);
    etTimeSys = tsManager.getKnownTimeSystem(TSManager.ET);
    taiTimeSys = tsManager.getKnownTimeSystem(TSManager.TAI);
    tdtTimeSys = tsManager.getKnownTimeSystem(TSManager.TDT);
  }

  public <Q> void addTimeSystem(TSId<Q> timeSystemId, TimeSystem<Q> ts) {
    tsManager.addTimeSystem(timeSystemId, ts);
  }

  public <Q> TimeSystem<Q> getKnownTimeSystem(TSId<Q> timeSystemId)
      throws UnknownTimeSystemException {
    return tsManager.getKnownTimeSystem(timeSystemId);
  }

  public TimeSystem<UTCEpoch> getUTCTimeSys() {
    return utcTimeSys;
  }

  public TimeSystem<Double> getETTimeSys() {
    return etTimeSys;
  }

  public TimeSystem<Double> getTAITimeSys() {
    return taiTimeSys;
  }

  /**
   * given an opaque TSEpoch, return the corresponding ET time (ET is same as TDB)
   */
  public double getET(TSEpoch tsEpoch) {
    return etTimeSys.getTime(tsEpoch);
  }


  /**
   * given an opaque TSEpoch, return the corresponding TAI time
   */
  public double getTAIJ2000(TSEpoch tsEpoch) {
    return taiTimeSys.getTime(tsEpoch);
  }

  /**
   * given an opaque TSEpoch, return the corresponding UTC time
   */
  public UTCEpoch getUTC(TSEpoch tsEpoch) {
    return utcTimeSys.getTime(tsEpoch);
  }

  /**
   * given an ET time, return an opaque TSEpoch
   */
  public TSEpoch getTSEpochFromET(double et) {
    return etTimeSys.getTSEpoch(et);
  }

  /**
   * given a time in UTC, return an opaque TSEpoch, return the corresponding UTC time
   */
  public TSEpoch getTSEpochFromUTC(UTCEpoch utc) {
    return utcTimeSys.getTSEpoch(utc);
  }


  public TSRange getTsRange(UTCEpoch t0, UTCEpoch t1) {
    return new TSRange(utcTimeSys.getTSEpoch(t0), utcTimeSys.getTSEpoch(t1));
  }

  public TSRange getTsRange(double et0, double et1) {
    return new TSRange(etTimeSys.getTSEpoch(et0), etTimeSys.getTSEpoch(et1));
  }

  /**
   * note: the doubles in the interval must be in ET (well, TDB)
   *
   * @param intervalInET TDB time range
   */
  public TSRange getTsRange(UnwritableInterval intervalInET) {
    return new TSRange(etTimeSys.getTSEpoch(intervalInET.getBegin()),
        etTimeSys.getTSEpoch(intervalInET.getEnd()));
  }

  public UnwritableInterval getUnwritableIntervalET(TSRange range) {
    return new UnwritableInterval(getET(range.getT0()), getET(range.getT1()));
  }

  public UnwritableInterval getUnwritableIntervalET(TSEpoch tse0, TSEpoch tse1) {
    return new UnwritableInterval(getET(tse0), getET(tse1));
  }

  public double convertToET(UTCEpoch utcEpoch) {
    return getET(getTSEpochFromUTC(utcEpoch));
  }

  public UTCEpoch convertToUTC(double et) {
    return getUTC(getTSEpochFromET(et));
  }

  public double convertTAIJ2000toTDB(double taiJ2000Secs) {
    return etTimeSys.getTime(taiTimeSys.getTSEpoch(taiJ2000Secs));
  }

  public double convertTDBtoTAIJ2000(double tdbSecs) {
    return taiTimeSys.getTime(etTimeSys.getTSEpoch(tdbSecs));
  }

  public double convertTDBToTDT(double tdb) {
    return tdtTimeSys.getTime(etTimeSys.getTSEpoch(tdb));
  }

  public double convertTDTToTDB(double tdt) {
    return etTimeSys.getTime(tdtTimeSys.getTSEpoch(tdt));
  }

  public <T> T getTime(TSId<T> id, TSEpoch tsEpoch) {
    return tsManager.getKnownTimeSystem(id).getTime(tsEpoch);
  }

  public <T> TSEpoch getTSEpoch(TSId<T> id, T time) {
    return tsManager.getKnownTimeSystem(id).getTSEpoch(time);
  }

  public String quickString(TSRange tsRange) {
    return getUTC(tsRange.getT0()).toString() + " to " + getUTC(tsRange.getT1()).toString();
  }


  public String quickFilenameSafeString(TSRange tsRange) {
    return getUTC(tsRange.getT0()).toString().replaceAll(":", "_") + "-"
        + getUTC(tsRange.getT1()).toString().replaceAll(":", "_");
  }

  public double getDurationInUTC(TSRange range) {
    return utcTimeSys.difference(range.getT1(), range.getT0());
  }

  public double getDurationInET(TSRange range) {
    return etTimeSys.difference(range.getT1(), range.getT0());
  };

  public TSEpoch addUTCSeconds(TSEpoch t, double deltaT) {
    return utcTimeSys.add(t, deltaT);
  }

  public TSEpoch addETSeconds(TSEpoch t, double deltaT) {
    return etTimeSys.add(t, deltaT);
  }


  /**
   * returns a time range that covers all time for which a UTC-based time mechanism could be
   * expected to know about (i.e., starting on day 1 of year 1 AD, to the future)
   *
   * @return
   */
  public TSRange getPostADAllUTCTime() {
    return new TSRange(getYearOneUTC(), TSEpoch.MAX_TIME);
  }

  /**
   * returns the TSEpoch value for the first day of year 1 on the UTC calendar
   *
   * @return
   */
  public TSEpoch getYearOneUTC() {
    return utcTimeSys.getTSEpoch(new UTCEpoch(1, 1, 0, 0, 0));
  }

  /**
   * a quick way to convert a TSEpoch to a string - do NOT use this for official output since this
   * moethod is not contract on the format of this string!!!
   *
   * @param tsEpoch
   * @return
   */
  public String quickString(TSEpoch tsEpoch) {
    return getUTC(tsEpoch).toString();
  }


  /**
   * Get a JAVA 8+ LocalDateTime object
   * 
   * @param epoch
   * @return
   */

  public LocalDateTime getLocalDateTime(TSEpoch epoch) {
    UTCEpoch utc = getUTC(epoch);
    double sec = utc.getSec();
    double seconds = Math.floor(sec);
    int nanoSeconds = (int) ((sec - seconds) * 1.e9);
    LocalDateTime dateTime = LocalDateTime.of(utc.getYear(), utc.getMonth(), utc.getDom(),
        utc.getHour(), utc.getMin(), (int) seconds, nanoSeconds);

    return dateTime;
  }

  /**
   * Get a TSEpoch from a Java 8+ LocalDateTime object
   * 
   * @param ldt
   * @return
   */
  public TSEpoch getTSEpochFromLocalDateTime(LocalDateTime ldt) {
    UTCEpoch utc = new UTCEpoch(ldt.getYear(), ldt.getDayOfYear(), ldt.getHour(), ldt.getMinute(),
        (ldt.getSecond()) + (1.e-9 * (ldt.getNano())));
    return getTSEpochFromUTC(utc);
  }

}
