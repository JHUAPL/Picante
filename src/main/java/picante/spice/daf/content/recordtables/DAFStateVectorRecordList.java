package picante.spice.daf.content.recordtables;

import picante.data.list.AbstractFixedLengthRetrievableWithExceptions;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;
import picante.spice.daf.DAFSegment;

public class DAFStateVectorRecordList
    extends AbstractFixedLengthRetrievableWithExceptions<StateVector> {

  /**
   * Each state contains position and velocity for a total length of six elements.
   */
  private static final int RECORD_SIZE = 6;

  /**
   * Local computational buffer used to populate individual components of the state vector record.
   */
  private final VectorIJK vector = new VectorIJK();

  /**
   * The DAF record retriever used to extract elements from the DAF.
   */
  private final DAFRecordRetriever retriever;

  public DAFStateVectorRecordList(DAFSegment segment, int length, int startIndex) {
    super(length);
    this.retriever = new DAFRecordRetriever(segment, length, startIndex, RECORD_SIZE);
  }

  @Override
  protected StateVector obtainRecord(int index, StateVector buffer) {
    double[] record = retriever.readRecord(index);

    vector.setTo(record[0], record[1], record[2]);
    buffer.setPosition(vector);

    vector.setTo(record[3], record[4], record[5]);
    buffer.setVelocity(vector);

    return buffer;
  }

}
