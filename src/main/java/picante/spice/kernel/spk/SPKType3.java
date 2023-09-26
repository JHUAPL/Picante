package picante.spice.kernel.spk;

import picante.data.list.Retrievable;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.kernel.utilities.DirectRecordIndexComputer;

/**
 * Implementation of the SPK type 3, Chebyshev position and velocity, ephemeris element.
 */
public class SPKType3 extends AbstractSPKSegment {

  private final DirectRecordIndexComputer indexComputer;

  /**
   * Local type 3 record to capture values read from the DAF.
   */
  private final SPKType3Record record = new SPKType3Record();

  /**
   * List of type 3 records containing the data content of the segment.
   */
  private final Retrievable<SPKType3Record> list;

  /**
   * Constructs a type 3 SPK segment from the supplied record list and meta data.
   * 
   * @param name the name of the SPK segment
   * @param targetID the integer ID code of the target, the head of the ephemeris vector
   * @param observerID the integer ID code of the observer, the tail of the ephemeris vector
   * @param frameID the integer ID code of the frame in which the ephemeris vector is expressed
   * @param startET seconds past J2000.0 in TDB indicating the start of the segment's applicability
   * @param finalET seconds past J2000.0 in TDB indicating the end of the segment's applicability
   * @param initialEpoch the start time for the first Chebyshev record in the list
   * @param intervalLength the length of the interval of applicability for each Chebyshev record
   * @param list the list of Chebyshev records
   */
  public SPKType3(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, double initialEpoch, double intervalLength,
      Retrievable<SPKType3Record> list) {
    super(name, targetID, observerID, frameID, startET, finalET);
    this.indexComputer = new DirectRecordIndexComputer(initialEpoch, intervalLength, list);
    this.list = list;
    // this.degree = (this.recordSize - 2) / 6 - 1;
  }

  @Override
  public int getType() {
    return 3;
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    /*
     * TODO: Implement caching scheme that prevents unnecessary lookups.
     */
    int recordIndex = indexComputer.computeRecordIndexForTime(time);
    list.get(recordIndex, record);
    return record.evaluate(time, buffer);
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    /*
     * TODO: Implement caching scheme that prevents unnecessary lookups.
     */
    int recordIndex = indexComputer.computeRecordIndexForTime(time);
    list.get(recordIndex, record);
    return record.evaluate(time, buffer);
  }

  public Retrievable<SPKType3Record> getRecordList() {
    return list;
  }

  public double getInitialEpoch() {
    return indexComputer.getInitialEpoch();
  }

  public double getIntervalLength() {
    return indexComputer.getIntervalLength();
  }

}
