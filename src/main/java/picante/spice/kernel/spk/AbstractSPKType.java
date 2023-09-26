package picante.spice.kernel.spk;

import com.google.common.annotations.VisibleForTesting;
import picante.data.list.GaugedRetrievable;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

/**
 * The purpose if this class is basically to retrieve the state vectors from the table, and then to
 * split up the state vectors into the appropriate arrays. This class tries to prevent excessive new
 * calls by holding on to buffers initially. The protected fields are meant to be used by the
 * subclass to
 * 
 * @author stephgk1
 * 
 */
abstract class AbstractSPKType extends AbstractSPKSegment {

  @VisibleForTesting
  final GaugedRetrievable<StateVector> table;
  @VisibleForTesting
  final StateVector[] bufferSV;
  @VisibleForTesting
  final FirstIndexRetriever firstIndexRetriever;

  protected final int recordSize;

  protected final double[] bufferTime;

  protected final double[] bufferX;
  protected final double[] bufferY;
  protected final double[] bufferZ;
  protected final double[] bufferdX;
  protected final double[] bufferdY;
  protected final double[] bufferdZ;

  /**
   * Constructs a type 9 SPK segment from the supplied record list and meta data.
   * 
   * @param name the name of the SPK segment
   * @param targetID the integer ID code of the target, the head of the ephemeris vector
   * @param observerID the integer ID code of the observer, the tail of the ephemeris vector
   * @param frameID the integer ID code of the frame in which the ephemeris vector is expressed
   * @param startET seconds past J2000.0 in TDB indicating the start of the segment's applicability
   * @param finalET seconds past J2000.0 in TDB indicating the end of the segment's applicability
   * @param recordSize The number of states which will be used to evaluate the function, this is
   *        sometimes called the window size.
   * 
   */
  public AbstractSPKType(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, GaugedRetrievable<StateVector> table, FirstIndexRetriever firstIndexRetriever,
      int recordSize) {
    super(name, targetID, observerID, frameID, startET, finalET);
    this.recordSize = recordSize;
    this.table = table;

    this.firstIndexRetriever = firstIndexRetriever;

    bufferTime = new double[recordSize];

    bufferSV = new StateVector[recordSize];
    for (int i = 0; i < recordSize; i++) {
      bufferSV[i] = new StateVector();
    }
    bufferX = new double[recordSize];
    bufferY = new double[recordSize];
    bufferZ = new double[recordSize];
    bufferdX = new double[recordSize];
    bufferdY = new double[recordSize];
    bufferdZ = new double[recordSize];

  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    prepareBuffers(time);
    return prepareUnivariatePosition(time, buffer);
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    prepareBuffers(time);
    return prepareUnivariateState(time, buffer);
  }

  @VisibleForTesting
  abstract VectorIJK prepareUnivariatePosition(double time, VectorIJK buffer);

  @VisibleForTesting
  abstract StateVector prepareUnivariateState(double time, StateVector buffer);

  @VisibleForTesting
  void prepareBuffers(double time) {
    int firstIndex = firstIndexRetriever.getFirstIndex(time, recordSize);
    fillTimeBuffer(firstIndex);
    fillStateVectorBuffers(getStateVectorRecord(firstIndex));
  }

  @VisibleForTesting
  StateVector[] getStateVectorRecord(int firstIndex) {

    for (int i = 0; i < recordSize; i++) {
      bufferSV[i] = table.get(i + firstIndex, bufferSV[i]);
    }

    return bufferSV;
  }

  @VisibleForTesting
  void fillTimeBuffer(int firstIndex) {

    for (int i = 0; i < recordSize; i++) {
      bufferTime[i] = table.getGauge(i + firstIndex);
    }

  }

  @VisibleForTesting
  void fillStateVectorBuffers(StateVector[] bufferSV) {
    for (int i = 0; i < recordSize; i++) {

      bufferX[i] = bufferSV[i].getPosition().getI();
      bufferY[i] = bufferSV[i].getPosition().getJ();
      bufferZ[i] = bufferSV[i].getPosition().getK();
      bufferdX[i] = bufferSV[i].getVelocity().getI();
      bufferdY[i] = bufferSV[i].getVelocity().getJ();
      bufferdZ[i] = bufferSV[i].getVelocity().getK();
    }
  }

}
