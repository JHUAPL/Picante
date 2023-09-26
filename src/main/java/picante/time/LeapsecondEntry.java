package picante.time;

import com.google.common.primitives.Doubles;

public final class LeapsecondEntry implements Comparable<LeapsecondEntry> {

  private final double dut;
  private final double formalEpoch;

  public LeapsecondEntry(double dut, double formalEpoch) {
    this.dut = dut;
    this.formalEpoch = formalEpoch;
  }

  public double getDut() {
    return dut;
  }

  public double getFormalEpoch() {
    return formalEpoch;
  }

  @Override
  public int compareTo(LeapsecondEntry o) {
    return Doubles.compare(this.formalEpoch, o.formalEpoch);

  }



}
