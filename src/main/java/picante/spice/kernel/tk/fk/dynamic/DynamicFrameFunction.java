package picante.spice.kernel.tk.fk.dynamic;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.StateTransform;

public interface DynamicFrameFunction extends Defineable {


  public int getFromID();

  public int getToID();

  public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer);

  public StateTransform getStateTransform(double time, StateTransform buffer);

}
