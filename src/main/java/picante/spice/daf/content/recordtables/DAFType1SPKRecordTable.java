package picante.spice.daf.content.recordtables;

import picante.data.list.AbstractFixedLengthGaugedRetrievableWithExceptions;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.spk.SPKType1Record;

/**
 * Implementation of the record table interface, backed by DAF, that provides records to an SPK type
 * 1 segment.
 */
public class DAFType1SPKRecordTable
    extends AbstractFixedLengthGaugedRetrievableWithExceptions<SPKType1Record> {

  /**
   * The size of a type 1 record is fixed at 71.
   */
  private final static int RECORD_SIZE = 71;

  /**
   * The time table, in parallel, to the record content.
   */
  private final DAFTimeListTable timeTable;

  private final DAFRecordRetriever retriever;

  /**
   * Create a DAF based type 1 SPK segment record table.
   * 
   * @param segment the DAF segment containing the data content
   * @param timeTable the table of times
   * @param startIndex specifies the index into the DAF at which the record content typically
   *        starts. For type 1 SPK segments this is almost always 0.
   */
  public DAFType1SPKRecordTable(DAFSegment segment, DAFTimeListTable timeTable, int startIndex) {
    super(timeTable.size());
    this.timeTable = timeTable;
    this.retriever = new DAFRecordRetriever(segment, timeTable.size(), startIndex, RECORD_SIZE);
  }

  @Override
  protected SPKType1Record obtainRecord(int index, SPKType1Record buffer) {
    buffer.setRecord(retriever.readRecord(index));
    return buffer;
  }

  @Override
  protected double obtainTime(int index) {
    return timeTable.getGauge(index);
  }

}
