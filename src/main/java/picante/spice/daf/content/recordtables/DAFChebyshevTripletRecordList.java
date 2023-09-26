package picante.spice.daf.content.recordtables;

import picante.data.list.AbstractFixedLengthRetrievableWithExceptions;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.utilities.ChebyshevTriplet;

/**
 * Implementation of the RecordList interface for a triplet of fixed record size Chebyshev
 * polynomials stored in a DAF. This code is utilized in both the type 2 SPK and type 2 PCK
 * implementations.
 * 
 * @param <T>
 */
public class DAFChebyshevTripletRecordList<T extends ChebyshevTriplet>
    extends AbstractFixedLengthRetrievableWithExceptions<T> {

  /**
   * The length of a record.
   */
  private final int recordSize;

  /**
   * The degree of each of the three Chebyshev polynomials.
   */
  private final int degree;

  /**
   * The record retriever used to extract blocks of doubles from the segment.
   */
  private final DAFRecordRetriever retriever;

  /**
   * Creates a Chebyshev triplet record list.
   * 
   * @param segment the DAF segment containing the data
   * @param numberOfRecords the number of records in the segment
   * @param startIndex the index into the segment at which the records start, typically 0
   * @param recordSize the size of an individual record
   */
  public DAFChebyshevTripletRecordList(DAFSegment segment, int numberOfRecords, int startIndex,
      int recordSize) {
    super(numberOfRecords);
    this.recordSize = recordSize;
    this.degree = (this.recordSize - 2) / 3 - 1;
    this.retriever = new DAFRecordRetriever(segment, numberOfRecords, startIndex, recordSize);
  }

  @Override
  protected T obtainRecord(int index, T buffer) {
    double[] record = retriever.readRecord(index);
    buffer.setCoefficients(degree, record, 2, record, 2 + degree + 1, record, 2 + 2 * (degree + 1),
        record, 0);
    return buffer;
  }

}
