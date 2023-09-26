package picante.time;

import java.util.HashMap;
import java.util.Map;
import picante.spice.kernel.tk.lsk.LSK;

/**
 * this class is designed to be the focus for obtaining the various time systems; time system
 * implementations are package private, and you only interact with them through the
 * {@link TimeSystem} interface, and the {@link TSManager} is the place where you obtain the time
 * system implementations that have been registered for different time basis types
 * 
 * IDs for three fundamental time systems are provided on the manager. If you want to add another
 * time system, the ID for that time system should be a public static final field on the
 * implementation class.
 * 
 * @author vandejd1
 */
public class TSManager {

  // Define some default time systems that we are guaranteed to know about
  public static final TSId<Double> ET = new TSId<Double>("ET");
  public static final TSId<TDBQuadTime> ETHiPrecision = new TSId<TDBQuadTime>("ETHighPrecision");
  public static final TSId<UTCEpoch> UTC = new TSId<UTCEpoch>("UTC");
  public static final TSId<Double> TAI = new TSId<Double>("TAI");
  public static final TSId<Double> TDT = new TSId<Double>("TDT");
  private final Map<TSId<?>, TimeSystem<?>> timeSystems = new HashMap<TSId<?>, TimeSystem<?>>();

  /**
   * you only get this through the static methods below that allow you to get a default instance or
   * let you create an empty one
   */
  private TSManager() {

  }

  public <Q> void addTimeSystem(TSId<Q> timeSystemId, TimeSystem<Q> ts) {
    timeSystems.put(timeSystemId, ts);
  }

  public boolean containsTimeSystem(TSId<?> timeSystemId) {
    return timeSystems.containsKey(timeSystemId);
  }

  @SuppressWarnings("unchecked")
  public <Q> TimeSystem<Q> getKnownTimeSystem(TSId<Q> timeSystemId) {
    TimeSystem<Q> ts = (TimeSystem<Q>) timeSystems.get(timeSystemId);
    if (ts == null) {
      throw new UnknownTimeSystemException(timeSystemId + " is unknown.");
    }
    return ts;
  }


  // TODO: figure out how to get SCLOCK types out of this manager/framework
  // @SuppressWarnings("unchecked")
  // public <P extends ITimeSystem<?>> P getKnownTimeSystem(TSId<?> timeSystemId, Class<P> clazz)
  // throws UnknownTimeSystemException {
  // P ts = (P) timeSystems.get(timeSystemId);
  // if (ts == null) {
  // throw new UnknownTimeSystemException(timeSystemId+" is unknown.");
  // }
  // return ts;
  // }


  public static TSManager createEmptyManager() {
    return new TSManager();
  }

  /**
   * Return a TSManager using built-in leap second parameters
   * 
   * @return
   */
  public static TSManager createStandardManager() {
    TSManager stdMgr = new TSManager();

    TimeSystems timeSystems = TimeSystems.createUsingInternalConstants();

    stdMgr.addTimeSystem(ET, timeSystems.getTDB());
    stdMgr.addTimeSystem(ETHiPrecision, timeSystems.getTDBQuadTime());
    stdMgr.addTimeSystem(UTC, timeSystems.getUTC());
    stdMgr.addTimeSystem(TAI, timeSystems.getTAI());
    stdMgr.addTimeSystem(TDT, timeSystems.getTDT());
    return stdMgr;
  }

  /**
   * Return a TSManager with its UTC time system created using the supplied {@link LSK}
   * 
   * @param lsk
   * @return
   */
  public static TSManager createStandardManager(LSK lsk) {
    double[] m = lsk.getM(new double[2]);
    TimeSystems.Builder tsBuilder = TimeSystems.builder();
    tsBuilder.withDeltaT_A(lsk.getDeltaTa());
    tsBuilder.withEB(lsk.getEB());
    tsBuilder.withK(lsk.getK());
    tsBuilder.withM0(m[0]);
    tsBuilder.withM1(m[1]);
    tsBuilder.withLeapseconds(lsk.getDeltaAT());
    TimeSystems timeSystems = tsBuilder.build();
    TSManager tsManager = TSManager.createStandardManager();
    // overwrite the UTC timesystem in the TSManager's map
    tsManager.addTimeSystem(TSManager.UTC, timeSystems.getUTC());

    return tsManager;
  }

  /**
   * a convenience method to create a standard manager and return a time system, which must be in
   * the standard manager
   * 
   * @param <T>
   * @param id
   * @return
   * @throws UnknownTimeSystemException
   */
  public static <T> TimeSystem<T> getTimeSystem(TSId<T> id) throws UnknownTimeSystemException {
    return createStandardManager().getKnownTimeSystem(id);
  }

  // TODO: to add:
  // 1. UnmodifiableSet<TSId<T>> getTimeSystems(Class<T> basisType)
  // must use .equals() to protect against subclass issues
  // on the basis type, even though we are adminstratively requiring
  // basis types to be final (no subclasses).
  // 2. getBasisTypes()
  //
  // 3. change the TSManager to return a Simplified Time system
}

// Modify the ITimeSystem to return not seconds, but ticks. Add a method: double
// getTicksPerSecond();
// Create an interface for a simplified time system that add seconds-based methods
