package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.spice.GeneralAberratedEphemerisProvider;

public interface Defineable {
  public void define(GeneralAberratedEphemerisProvider generalProvider,
      Map<Integer, FrameID> frameIDMap, Map<Integer, EphemerisID> ephemerisIDMap)
      throws DynamicFrameDefinitionException;

}
