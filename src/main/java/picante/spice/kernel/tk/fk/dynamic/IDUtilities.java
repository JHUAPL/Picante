package picante.spice.kernel.tk.fk.dynamic;

import java.util.Map;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;

public class IDUtilities {
  public static FrameID code2FrameID(int frameCode, Map<Integer, FrameID> frameIDMap)
      throws DynamicFrameDefinitionException {
    if (!frameIDMap.containsKey(frameCode)) {
      throw new DynamicFrameDefinitionException("Frame not contained in FrameID Map: " + frameCode);
    }
    FrameID frameID = frameIDMap.get(frameCode);
    return frameID;
  }

  public static EphemerisID code2EphemerisID(int ephemerisCode,
      Map<Integer, EphemerisID> ephemerisIDMap) throws DynamicFrameDefinitionException {
    if (!ephemerisIDMap.containsKey(ephemerisCode)) {
      throw new DynamicFrameDefinitionException(
          "Ephemeris not contained in EphemerisID Map: " + ephemerisCode);
    }
    EphemerisID frameID = ephemerisIDMap.get(ephemerisCode);
    return frameID;
  }
}
