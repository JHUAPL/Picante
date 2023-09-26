package picante.spice.kernel.tk.fk;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

public class TKFrameFunction {

  private final int fromID;
  private final int toID;

  private final UnwritableRotationMatrixIJK rotation;

  public TKFrameFunction(int fromID, int toID, UnwritableRotationMatrixIJK rotation) {
    this.rotation = new UnwritableRotationMatrixIJK(rotation);
    this.fromID = fromID;
    this.toID = toID;
  }

  public int getFromID() {
    return fromID;
  }

  public int getToID() {
    return toID;
  }

  public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double time,
      RotationMatrixIJK buffer) {
    return buffer.setTo(rotation);
  }

}
