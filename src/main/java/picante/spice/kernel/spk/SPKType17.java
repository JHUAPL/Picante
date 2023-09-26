package picante.spice.kernel.spk;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

public class SPKType17 extends AbstractSPKSegment {

  private final EquinoctialElements elements;

  public SPKType17(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, double[] record) {
    super(name, targetID, observerID, frameID, startET, finalET);
    checkArgument(record.length == 12, "SPK Type 17 DAF record must be exactly 12 elements long.");

    elements = new EquinoctialElements(record[0], record[1], record[2], record[3], record[4],
        record[5], record[6], record[7], record[8], record[9], record[10], record[11]);

  }

  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    elements.evaluate(time, buffer);
    return buffer;
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    elements.evaluate(time, buffer);
    return buffer;
  }

  @Override
  public int getType() {
    return 17;
  }

}
