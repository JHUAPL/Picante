package picante.spice.kernel.spk;

import picante.data.list.GaugedRetrievableLLT;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

/**
 * Implementation of the SPK type 1, modified difference array, ephemeris element.
 */
public class SPKType1 extends AbstractSPKSegment {

  /**
   * Local type 1 record to capture values read from the DAF.
   */
  private final SPKType1Record record = new SPKType1Record();

  /**
   * Local state vector buffer to cache computation results.
   */
  private final StateVector stateBuffer = new StateVector();

  private final GaugedRetrievableLLT<SPKType1Record> table;

  /**
   * Constructs a type 1 segment from the supplied DAF segment.
   * 
   * @param segment a DAF segment with SPK type 1 content
   */
  public SPKType1(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, GaugedRetrievableLLT<SPKType1Record> table) {
    super(name, targetID, observerID, frameID, startET, finalET);
    this.table = table;
  }

  @Override
  public int getType() {
    return 1;
  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    getState(time, stateBuffer);
    buffer.setTo(stateBuffer.getPosition());
    return buffer;
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    /*
     * TODO: Implement record and time caching to improve performance
     * 
     * Note, we want the record whose epoch is greater than or equal to the supplied time, hence add
     * one to the value returned from the last less than search.
     */
    int recordIndex = table.indexLastLessThan(time) + 1;
    table.get(recordIndex, record);
    return record.evaluate(time, buffer);
  }

  public GaugedRetrievableLLT<SPKType1Record> getRecordTable() {
    return table;
  }

}
