package picante.spice.daf.content.recordtables;

import picante.data.list.AbstractFixedLengthRetrievableWithExceptions;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.spk.SPKType3Record;

/**
 * Implementation of the RecordList interface for the type 3 SPK segment stored in a DAF.
 * <p>
 * Note: while this class extends the abstract DAF record table, which provides a fully serviced
 * record table with both LLT and LLE searches, invoking these methods results in uncertain
 * behavior. This class is only to provide the record list interface.
 * </p>
 */
public class DAFType3SPKRecordList
    extends AbstractFixedLengthRetrievableWithExceptions<SPKType3Record> {

  /**
   * The length of a record is fixed at 2 + 6*(degree+1), as each polynomial from a specific table
   * must have the same degree.
   */
  private final int recordSize;

  /**
   * The degree of the polynomials captured in the record list.
   */
  private final int degree;

  private final DAFRecordRetriever retriever;

  /**
   * Creates a DAF type 3 SPK record list.
   * 
   * @param segment the DAF segment containing the data
   * @param numberOfRecords the number of records in the segment
   * @param startIndex the index into the segment at which the records start, typically 0
   * @param recordSize the size of an individual record
   */
  public DAFType3SPKRecordList(DAFSegment segment, int numberOfRecords, int startIndex,
      int recordSize) {
    super(numberOfRecords);
    this.recordSize = recordSize;
    this.degree = (this.recordSize - 2) / 6 - 1;
    this.retriever = new DAFRecordRetriever(segment, numberOfRecords, startIndex, recordSize);
  }

  @Override
  protected SPKType3Record obtainRecord(int index, SPKType3Record buffer) {

    double[] record = retriever.readRecord(index);

    buffer.setCoefficients(degree, record, 2, record, 2 + (degree + 1), record,
        2 + 2 * (degree + 1), record, 2 + 3 * (degree + 1), record, 2 + 4 * (degree + 1), record,
        2 + 5 * (degree + 1), record, 0);

    return buffer;
  }

}
