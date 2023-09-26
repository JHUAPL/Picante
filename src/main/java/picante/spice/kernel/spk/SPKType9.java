package picante.spice.kernel.spk;

import com.google.common.annotations.VisibleForTesting;
import picante.data.list.GaugedRetrievableLLT;
import picante.math.functions.LagrangePolynomial;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

/**
 * Implementation of the SPK type 9, Lagrange interpolation, unequal time steps.
 * 
 * @author stephgk1
 * 
 */
public class SPKType9 extends AbstractSPKType {

  private final static int TYPE = 9;
  private final LagrangePolynomial function = new LagrangePolynomial();

  /**
   * Constructs a type 9 SPK segment from the supplied record table and meta data.
   * 
   * @param name the name of the SPK segment
   * @param targetID the integer ID code of the target, the head of the ephemeris vector
   * @param observerID the integer ID code of the observer, the tail of the ephemeris vector
   * @param frameID the integer ID code of the frame in which the ephemeris vector is expressed
   * @param startET seconds past J2000.0 in TDB indicating the start of the segment's applicability
   * @param finalET seconds past J2000.0 in TDB indicating the end of the segment's applicability
   * @param table a RecordTable of StateVectors containing the records and ephemeris.
   * @param recordSize the number of records used in the evaluation of the Lagrange polynomial
   */
  public SPKType9(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, GaugedRetrievableLLT<StateVector> table, int recordSize) {
    super(name, targetID, observerID, frameID, startET, finalET, table,
        new FirstIndexRetrieverTable<StateVector>(table), recordSize);
  }

  @Override
  @VisibleForTesting
  VectorIJK prepareUnivariatePosition(double time, VectorIJK buffer) {
    function.setDataPoints(recordSize, bufferTime, bufferX);
    double x = function.evaluate(time);

    function.setDataPoints(recordSize, bufferTime, bufferY);
    double y = function.evaluate(time);

    function.setDataPoints(recordSize, bufferTime, bufferZ);
    double z = function.evaluate(time);

    return buffer.setTo(x, y, z);
  }

  @Override
  @VisibleForTesting
  StateVector prepareUnivariateState(double time, StateVector buffer) {
    function.setDataPoints(recordSize, bufferTime, bufferdX);
    double dx = function.evaluate(time);

    function.setDataPoints(recordSize, bufferTime, bufferdY);
    double dy = function.evaluate(time);

    function.setDataPoints(recordSize, bufferTime, bufferdZ);
    double dz = function.evaluate(time);

    buffer.setPosition(prepareUnivariatePosition(time, buffer.getPosition()));
    buffer.setVelocity(buffer.getVelocity().setTo(dx, dy, dz));

    return buffer;
  }

  public int getDegree() {
    return recordSize - 1;
  }

  @Override
  public int getType() {
    return TYPE;
  }

}
