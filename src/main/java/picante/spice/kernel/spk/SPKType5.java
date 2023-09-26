package picante.spice.kernel.spk;

import static com.google.common.base.Preconditions.checkNotNull;
import picante.data.list.GaugedRetrievableLLT;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateVector;

public class SPKType5 extends AbstractSPKSegment {

  private final static int TYPE = 5;
  private final GaugedRetrievableLLT<StateVector> states;
  private final double gm;

  private final SPKType5Evaluator evaluator = new SPKType5Evaluator();
  private final StateVector left = new StateVector();
  private final StateVector right = new StateVector();

  public SPKType5(String name, int targetID, int observerID, int frameID, double startET,
      double finalET, GaugedRetrievableLLT<StateVector> recordList, double gm) {
    super(name, targetID, observerID, frameID, startET, finalET);
    this.states = checkNotNull(recordList);
    this.gm = gm;
  }


  private void populateBuffers(double time) {

    int index = states.indexLastLessThan(time);

    /*
     * If the time occurs prior to all records in the state table, then assign both states to the
     * first state in the table.
     */
    if (index == -1) {
      double recordTime = states.getGauge(0);
      states.get(0, left);
      evaluator.configure(gm, recordTime, left, recordTime, left);
      return;
    }

    /*
     * If time occurs after all records in the state table, then assign both states to the last
     * state in the table.
     */
    if (index == (states.size() - 1)) {
      double recordTime = states.getGauge(states.size() - 1);
      states.get(states.size() - 1, left);
      evaluator.configure(gm, recordTime, left, recordTime, right);
      return;
    }

    /*
     * Otherwise, assign the bracketing states.
     */
    double leftTdb = states.getGauge(index);
    double rightTdb = states.getGauge(index + 1);
    states.get(index, left);
    states.get(index + 1, right);
    evaluator.configure(gm, leftTdb, left, rightTdb, right);

  }


  @Override
  public VectorIJK getPosition(double time, VectorIJK buffer) {
    populateBuffers(time);
    evaluator.evaluate(time, left);
    return buffer.setTo(left.getPosition());
  }

  @Override
  public StateVector getState(double time, StateVector buffer) {
    populateBuffers(time);
    return evaluator.evaluate(time, buffer);
  }



  @Override
  public int getType() {
    return TYPE;
  }



}
