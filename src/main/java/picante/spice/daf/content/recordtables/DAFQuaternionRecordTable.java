package picante.spice.daf.content.recordtables;

import picante.data.list.AbstractFixedLengthGaugedRetrievableWithExceptions;
import picante.mechanics.rotations.Quaternion;
import picante.spice.daf.DAFSegment;

/**
 * Implementation of the record table interface, backed by DAF, that provides records of quaternions
 * in the standard NAIF ordering.
 */
public class DAFQuaternionRecordTable
    extends AbstractFixedLengthGaugedRetrievableWithExceptions<Quaternion> {

  /**
   * Quaternions have four double precision components.
   */
  private static final int RECORD_SIZE = 4;

  /**
   * The time table, in parallel, to the record content.
   */
  private final DAFTimeListTable timeTable;

  /**
   * DAF block record retriever used to extract record data from the segment.
   */
  private final DAFRecordRetriever retriever;

  /**
   * Create a DAF based quaternion record table
   * 
   * @param segment the DAF segment containing the data content
   * @param timeTable the table of times
   * @param startIndex specifies the index into the DAF at which the record content starts. For most
   *        C-kernel segments this would be 0.
   */
  public DAFQuaternionRecordTable(DAFSegment segment, DAFTimeListTable timeTable, int startIndex) {
    super(timeTable.size());
    this.timeTable = timeTable;
    this.retriever = new DAFRecordRetriever(segment, timeTable.size(), startIndex, RECORD_SIZE);
  }

  @Override
  protected Quaternion obtainRecord(int index, Quaternion buffer) {
    double[] record = retriever.readRecord(index);
    buffer.setTo(record[0], record[1], record[2], record[3]);
    return buffer;
  }

  @Override
  protected double obtainTime(int index) {
    return timeTable.getGauge(index);
  }

}
