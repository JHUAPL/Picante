package picante.time;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * A wrapper of TSRange that is serializable.
 * 
 * @author peachjm1
 *
 */
public class TSRangeSerializable implements Serializable {
  private static final long serialVersionUID = -6909944342458518367L;

  public static TSRangeSerializable of(TSRange timeRange) {
    return new TSRangeSerializable(timeRange);
  }

  private transient TSRange timeRange;

  protected TSRangeSerializable(TSRange timeRange) {
    if (timeRange == null || timeRange.getT0() == null || timeRange.getT1() == null) {
      throw new NullPointerException();
    }
    this.timeRange = timeRange;
  }

  public TSRange get() {
    return timeRange;
  }

  private void readObject(ObjectInputStream ois) throws ClassNotFoundException, IOException {
    TSEpoch t0 = (TSEpoch) ois.readObject();
    TSEpoch t1 = (TSEpoch) ois.readObject();
    timeRange = new TSRange(t0, t1);
  }

  private void writeObject(ObjectOutputStream oos) throws IOException {
    oos.writeObject(timeRange.getT0());
    oos.writeObject(timeRange.getT1());
  }
}
