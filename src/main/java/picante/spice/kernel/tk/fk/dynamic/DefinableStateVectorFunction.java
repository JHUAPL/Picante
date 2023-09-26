package picante.spice.kernel.tk.fk.dynamic;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.mechanics.UnwritableStateVector;

public interface DefinableStateVectorFunction extends Defineable {

  public UnwritableVectorIJK getVector(double time);

  public UnwritableStateVector getStateVector(double time);

}
