package picante.spice.daf.content.recordtables;

import picante.data.list.AbstractFixedLengthGaugedRetrievableWithExceptions;
import picante.mechanics.rotations.Quaternion;
import picante.mechanics.rotations.WrapperWithRate;
import picante.spice.daf.DAFSegment;

/**
 * Implementation of the record table interface, backed by DAF, for the quaternion and angular
 * rates. The record ordering consists of (q0, q1, q2, q3, av1, av2, av3).
 */
public class DAFQuaternionAVRecordTable
    extends AbstractFixedLengthGaugedRetrievableWithExceptions<WrapperWithRate<Quaternion>> {

  /**
   * Each quaternion has four components, the angular velocity 3.
   */
  private final static int RECORD_SIZE = 7;

  /**
   * The time table, in parallel, to the record content.
   */
  private final DAFTimeListTable timeTable;

  /**
   * DAF block record retriever used to extract data from the segment.
   */
  private final DAFRecordRetriever retriever;

  /**
   * Create a DAF based quaternion and angular velocity record table
   * 
   * @param segment the DAF segment containing the data content
   * @param timeTable the table of times
   * @param startIndex specifies the index into the DAF at which the record content starts. For most
   *        C-kernel segments this would be 0.
   */
  public DAFQuaternionAVRecordTable(DAFSegment segment, DAFTimeListTable timeTable,
      int startIndex) {
    super(timeTable.size());
    this.timeTable = timeTable;
    this.retriever = new DAFRecordRetriever(segment, timeTable.size(), startIndex, RECORD_SIZE);
  }

  @Override
  protected WrapperWithRate<Quaternion> obtainRecord(int index,
      WrapperWithRate<Quaternion> buffer) {
    double[] record = retriever.readRecord(index);
    buffer.getRotation().setTo(record[0], record[1], record[2], record[3]);
    buffer.getRate().setTo(record[4], record[5], record[6]);
    return buffer;
  }

  @Override
  protected double obtainTime(int index) {
    return timeTable.getGauge(index);
  }

}
