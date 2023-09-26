package picante.spice;

import java.util.Map;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.providers.aberrated.AberratedEphemerisProvider;

public class GeneralAberratedEphemerisProvider {
  private final AberratedEphemerisProvider singleProvider;
  private final AberratedEphemerisProvider tripleProvider;

  public GeneralAberratedEphemerisProvider(EphemerisAndFrameProvider provider,
      Map<? extends FrameID, ? extends EphemerisID> frameCenterMap) {
    singleProvider = AberratedEphemerisProvider.createSingleIteration(provider, frameCenterMap);
    tripleProvider = AberratedEphemerisProvider.createTripleIteration(provider, frameCenterMap);
  }

  public AberratedEphemerisProvider getProvider(boolean triple) {
    if (triple) {
      return tripleProvider;
    } else {
      return singleProvider;
    }
  };
}
