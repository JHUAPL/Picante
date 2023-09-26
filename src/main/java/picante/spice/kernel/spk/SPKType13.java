package picante.spice.kernel.spk;

import com.google.common.annotations.Beta;
import com.google.common.annotations.VisibleForTesting;
import picante.data.list.GaugedRetrievableLLT;
import picante.math.functions.HermitePolynomial;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

/**
 * Implementation of the SPK type 13, Hermite interpolation, unequal time steps.
 * 
 * @author stephgk1
 * 
 */
@Beta
public class SPKType13 extends AbstractSPKType {

  private final static int TYPE = 13;
  private final HermitePolynomial function = new HermitePolynomial();

  private final double[] bufferXdX;
  private final double[] bufferYdY;
  private final double[] bufferZdZ;
  private final double[] bX;
  private final double[] bY;
  private final double[] bZ;

  /**
   * Constructs a type 13 SPK segment from the supplied record table and meta data.
   * 
   * @param name the name of the SPK segment
   * @param targetID the integer ID code of the target, the head of the ephemeris vector
   * @param observerID the integer ID code of the observer, the tail of the ephemeris vector
   * @param frameID the integer ID code of the frame in which the ephemeris vector is expressed
   * @param startET seconds past J2000.0 in TDB indicating the start of the segment's applicability
   * @param finalET seconds past J2000.0 in TDB indicating the end of the segment's applicability
   * @param table a RecordTable of StateVectors containing the records and ephemeris.
   * @param recordSize the number of records used in the evaluation of the Hermite polynomial
   */
  public SPKType13(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, GaugedRetrievableLLT<StateVector> table, int recordSize) {
    super(name, targetID, observerID, frameID, startET, finalET, table,
        new FirstIndexRetrieverTable<StateVector>(table), recordSize);
    bufferXdX = new double[recordSize * 2];
    bufferYdY = new double[recordSize * 2];
    bufferZdZ = new double[recordSize * 2];
    bX = new double[2];
    bY = new double[2];
    bZ = new double[2];
  }

  @Override
  VectorIJK prepareUnivariatePosition(double time, VectorIJK buffer) {
    fillDoubleBuffers();
    function.setPoints(recordSize, bufferTime, bufferXdX);
    function.evaluate(time, bX);

    function.setPoints(recordSize, bufferTime, bufferYdY);
    function.evaluate(time, bY);

    function.setPoints(recordSize, bufferTime, bufferZdZ);
    function.evaluate(time, bZ);

    return buffer.setTo(bX[0], bY[0], bZ[0]);
  }

  @Override
  StateVector prepareUnivariateState(double time, StateVector buffer) {
    buffer.setPosition(prepareUnivariatePosition(time, buffer.getPosition()));
    buffer.setVelocity(buffer.getVelocity().setTo(bX[1], bY[1], bZ[1]));
    return buffer;
  }

  @VisibleForTesting
  void fillDoubleBuffers() {
    for (int i = 0; i < recordSize; i++) {
      bufferXdX[2 * i] = bufferX[i];
      bufferXdX[2 * i + 1] = bufferdX[i];
      bufferYdY[2 * i] = bufferY[i];
      bufferYdY[2 * i + 1] = bufferdY[i];
      bufferZdZ[2 * i] = bufferZ[i];
      bufferZdZ[2 * i + 1] = bufferdZ[i];
    }
  }

  public int getDegree() {
    return 2 * recordSize - 1;
  }

  @Override
  public int getType() {
    return TYPE;
  }

}
