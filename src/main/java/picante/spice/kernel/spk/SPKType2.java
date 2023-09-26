package picante.spice.kernel.spk;

import picante.data.list.Retrievable;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.kernel.utilities.DirectRecordIndexComputer;

/**
 * Implementation of the SPK type 2, Chebyshev position only, ephemeris element.
 */
public class SPKType2 extends AbstractSPKSegment {

  /**
   * Class to compute the index from a given time.
   */
  private final DirectRecordIndexComputer indexComputer;

  /**
   * Local type 2 record to capture values read from the DAF.
   */
  private final SPKType2Record record = new SPKType2Record();

  /**
   * List of type 2 records containing the data content of the segment.
   */
  private final Retrievable<SPKType2Record> list;

  /**
   * Constructs a type 2 SPK segment from the supplied record list and meta data.
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
  public SPKType2(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, double initialEpoch, double intervalLength,
      Retrievable<SPKType2Record> list) {
    super(name, targetID, observerID, frameID, startET, finalET);
    this.indexComputer = new DirectRecordIndexComputer(initialEpoch, intervalLength, list);
    this.list = list;
  }

  @Override
  public int getType() {
    return 2;
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

  public Retrievable<SPKType2Record> getRecordList() {
    return list;
  }

  public double getInitialEpoch() {
    return indexComputer.getInitialEpoch();
  }

  public double getIntervalLength() {
    return indexComputer.getIntervalLength();
  }

}
