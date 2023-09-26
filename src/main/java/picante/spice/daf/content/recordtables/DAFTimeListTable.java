package picante.spice.daf.content.recordtables;

import picante.data.list.AbstractFixedLengthGaugedRetrievableWithExceptions;
import picante.spice.daf.DAFSegment;

/**
 * Provides an implementation of the record table interface designed to allow direct querying on a
 * block of doubles stored in a DAF segment that capture a non-decreasing sequence of times. This
 * class can easily double as a record list of times as well, if necessary.
 */
public class DAFTimeListTable extends AbstractFixedLengthGaugedRetrievableWithExceptions<Object> {

  private static final int RECORD_SIZE = 1;

  private final DAFRecordRetriever retriever;

  /**
   * Creates a DAF time list.
   * 
   * @param segment the DAF segment containing the data
   * @param length length of the list
   * @param startIndex index into the DAF segment where the list starts
   */
  public DAFTimeListTable(DAFSegment segment, int length, int startIndex) {
    super(length);
    this.retriever = new DAFRecordRetriever(segment, length, startIndex, RECORD_SIZE);
  }

  /**
   * This method should never be invoked, as this record table exists only to provide a list of
   * times.
   */
  @Override
  protected Object obtainRecord(@SuppressWarnings("unused") int index,
      @SuppressWarnings("unused") Object buffer) {
    throw new UnsupportedOperationException(
        "This specific record table only captures a list of times.");
  }

  @Override
  protected double obtainTime(int index) {
    double[] record = retriever.readRecord(index);
    return record[0];
  }

}
