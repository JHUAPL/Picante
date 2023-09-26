package picante.spice.kernel.pck;

import picante.data.list.Retrievable;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.rotations.DifferentiatedEulerAngles;
import picante.spice.kernel.utilities.DirectRecordIndexComputer;

/**
 * Implementation of the PCK type 2, Chebyshev angles only, binary PCK element.
 * <p>
 * Note: this implementation is <b>not</b> complete as it does not produce proper state
 * transformations. This was an acceptable reduction in scope, as only the rotation itself was
 * needed at the time it was put together.
 * </p>
 */
public class PCKType2 extends AbstractPCKSegment {

  /**
   * Class to compute the index from a given time.
   */
  private final DirectRecordIndexComputer indexComputer;

  /**
   * Local type 2 record to capture values read from the DAF.
   */
  private final PCKType2Record record = new PCKType2Record();

  /**
   * EulerAngles class that supports the computation of the rotation.
   */
  private final DifferentiatedEulerAngles.KIK angles = new DifferentiatedEulerAngles.KIK();

  /**
   * List of type 2 records containing the data content of the segment.
   */
  private final Retrievable<PCKType2Record> list;

  /**
   * Constructs a type 2 PCK segment from the supplied record list and meta data.
   * 
   * @param name the name of the PCK segment.
   * @param bodyFrameID the integer ID code of the body frame, the &quot;to&quot; frame of the state
   *        transformation
   * @param referenceFrameID the integer ID code of the reference frame, the &quot;from&quot; frame
   *        of the state transformation
   * @param startET seconds past J2000.0 in TDB indicating the start of the segment's applicability
   * @param finalET seconds past J2000.0 in TDB indicating the end of the segment's applicability
   * @param initialEpoch the start time for the first Chebyshev triplet in the list
   * @param intervalLength the length of the interval of applicability for each Chebyshev record
   * @param list the list of Chebyshev records
   */
  public PCKType2(String name, int bodyFrameID, int referenceFrameID, double startET,
      double finalET, double initialEpoch, double intervalLength,
      Retrievable<PCKType2Record> list) {
    super(name, bodyFrameID, referenceFrameID, startET, finalET);
    indexComputer = new DirectRecordIndexComputer(initialEpoch, intervalLength, list);
    this.list = list;

  }

  @Override
  public int getType() {
    return 2;
  }

  @Override
  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
    int recordIndex = indexComputer.computeRecordIndexForTime(time);
    list.get(recordIndex, record);
    record.evaluate(time, angles.getRotation());
    return angles.getRotation().getRotation(buffer);
  }

  @Override
  public StateTransform getTransform(double time, StateTransform buffer) {
    int recordIndex = indexComputer.computeRecordIndexForTime(time);
    list.get(recordIndex, record);
    record.evaluate(time, angles);
    return angles.getTransform(buffer);
  }

  /**
   * Retrieves the record list supporting this segment.
   * 
   * @return a reference to the record list of supporting data.
   */
  public Retrievable<PCKType2Record> getRecordList() {
    return list;
  }

  /**
   * Retrieves the time of the initial record in the list.
   * 
   * @return TDB seconds past J2000.
   */
  public double getInitialEpoch() {
    return indexComputer.getInitialEpoch();
  }

  /**
   * Retrieves the time difference between two subsequent records in the list.
   * 
   * @return TDB seconds
   */
  public double getIntervalLength() {
    return indexComputer.getIntervalLength();
  }

}
