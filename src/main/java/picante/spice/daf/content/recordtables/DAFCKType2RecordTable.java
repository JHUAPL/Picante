package picante.spice.daf.content.recordtables;

import static com.google.common.base.Preconditions.checkArgument;
import picante.data.list.AbstractRetrievableWithExceptions;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.ck.CKType2Record;

public class DAFCKType2RecordTable extends AbstractRetrievableWithExceptions<CKType2Record> {

  /**
   * Quaternions have four double precision components, followed by the angular rate of three
   * components, and a single double that captures the tick rate.
   */
  private static final int RECORD_SIZE = 8;

  private final int size;

  private final DAFRecordRetriever retriever;

  public DAFCKType2RecordTable(DAFSegment segment, int size) {
    checkArgument(size > 0, "Size: %s is invalid, must be strictly positive.");
    this.size = size;
    /*
     * The attitude records in a type 2 segment always start at the beginning of the segment.
     */
    this.retriever = new DAFRecordRetriever(segment, size, 0, RECORD_SIZE);
  }

  @Override
  public int size() {
    return size;
  }

  @Override
  protected CKType2Record obtain(int index, CKType2Record buffer) {
    double[] record = retriever.readRecord(index);
    buffer.setRecord(record[0], record[1], record[2], record[3], record[4], record[5], record[6],
        record[7]);
    return buffer;
  }


}
