package picante.mechanics;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;

public class CelestialFramesTest {

  @Test
  public void testFrameCenterMap() {

    /*
     * Verify that every frame is represented in the FRAME_CENTER_MAP.
     */
    assertTrue(Sets.difference(ImmutableSet.copyOf(CelestialFrames.values()),
        CelestialFrames.FRAME_CENTER_MAP.keySet()).isEmpty());
    assertTrue(Sets.difference(CelestialFrames.FRAME_CENTER_MAP.keySet(),
        ImmutableSet.copyOf(CelestialFrames.values())).isEmpty());

    /*
     * For frames with names starting with IAU_ make certain that they map to the IAU_ stripped off
     * entry in CelestialBodies.
     */
    for (CelestialFrames frame : CelestialFrames.FRAME_CENTER_MAP.keySet()) {

      if (frame.toString().matches("IAU_.*")) {
        assertEquals(frame.toString().replace("IAU_", ""),
            CelestialFrames.FRAME_CENTER_MAP.get(frame).toString());
      }
    }
  }
}
