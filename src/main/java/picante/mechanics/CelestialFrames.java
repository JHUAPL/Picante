package picante.mechanics;

import com.google.common.collect.ImmutableMap;

/**
 * This enumeration provides an implementation of the FrameID interface for all of the built-in
 * (recognized or standard) celestial frames.
 * <p>
 * At the moment it includes all of the inertial frames defined in the SPICE Toolkit, and the
 * built-in body-fixed frame definitions.
 * </p>
 */
public enum CelestialFrames implements FrameID {

  J2000(true), B1950(true), FK4(true), DE118(true), DE96(true), DE102(true), DE108(true), DE111(
      true), DE114(true), DE122(true), DE125(true), DE130(true), GALACTIC(true), DE200(true), DE202(
          true), MARSIAU(true), ECLIPJ2000(true), ECLIPB1950(true), DE140(true), DE142(true), DE143(
              true), IAU_MERCURY_BARYCENTER, IAU_VENUS_BARYCENTER, IAU_EARTH_BARYCENTER, IAU_MARS_BARYCENTER, IAU_JUPITER_BARYCENTER, IAU_SATURN_BARYCENTER, IAU_URANUS_BARYCENTER, IAU_NEPTUNE_BARYCENTER, IAU_PLUTO_BARYCENTER, IAU_SUN, IAU_MERCURY, IAU_VENUS, IAU_EARTH, IAU_MARS, IAU_JUPITER, IAU_SATURN, IAU_URANUS, IAU_NEPTUNE, IAU_PLUTO, IAU_MOON, IAU_PHOBOS, IAU_DEIMOS, IAU_IO, IAU_EUROPA, IAU_GANYMEDE, IAU_CALLISTO, IAU_AMALTHEA, IAU_HIMALIA, IAU_ELARA, IAU_PASIPHAE, IAU_SINOPE, IAU_LYSITHEA, IAU_CARME, IAU_ANANKE, IAU_LEDA, IAU_THEBE, IAU_ADRASTEA, IAU_METIS, IAU_MIMAS, IAU_ENCELADUS, IAU_TETHYS, IAU_DIONE, IAU_RHEA, IAU_TITAN, IAU_HYPERION, IAU_IAPETUS, IAU_PHOEBE, IAU_JANUS, IAU_EPIMETHEUS, IAU_HELENE, IAU_TELESTO, IAU_CALYPSO, IAU_ATLAS, IAU_PROMETHEUS, IAU_PANDORA, IAU_ARIEL, IAU_UMBRIEL, IAU_TITANIA, IAU_OBERON, IAU_MIRANDA, IAU_CORDELIA, IAU_OPHELIA, IAU_BIANCA, IAU_CRESSIDA, IAU_DESDEMONA, IAU_JULIET, IAU_PORTIA, IAU_ROSALIND, IAU_BELINDA, IAU_PUCK, IAU_TRITON, IAU_NEREID, IAU_NAIAD, IAU_THALASSA, IAU_DESPINA, IAU_GALATEA, IAU_LARISSA, IAU_PROTEUS, IAU_CHARON, EARTH_FIXED, IAU_PAN, IAU_GASPRA, IAU_IDA, IAU_EROS, IAU_CALLIRRHOE, IAU_THEMISTO, IAU_MEGACLITE, IAU_TAYGETE, IAU_CHALDENE, IAU_HARPALYKE, IAU_KALYKE, IAU_IOCASTE, IAU_ERINOME, IAU_ISONOE, IAU_PRAXIDIKE, IAU_BORRELLY, IAU_TEMPEL_1, IAU_VESTA, IAU_ITOKAWA, IAU_CERES, IAU_PALLAS, IAU_LUTETIA, IAU_DAVIDA, IAU_STEINS, IAU_BENNU, IAU_A52_EUROPA, IAU_NIX, IAU_HYDRA, IAU_RYUGU, IAU_ARROKOTH, IAU_DIDYMOS_BARYCENTER, IAU_DIDYMOS, IAU_DIMORPHOS, IAU_DONALDJOHANSON, IAU_EURYBATES, IAU_EURYBATES_BARYCENTER, IAU_QUETA, IAU_POLYMELE, IAU_LEUCUS, IAU_ORUS, IAU_PATROCLUS_BARYCENTER, IAU_PATROCLUS, IAU_MENOETIUS, ITRF93;

  private final boolean inertial;

  private CelestialFrames() {
    this.inertial = false;
  }

  private CelestialFrames(boolean inertial) {
    this.inertial = inertial;
  }

  @Override
  public String getName() {
    return name();
  }

  @Override
  public boolean isInertial() {
    return inertial;
  }

  /**
   * Mapping of frames to their natural &quot;centers&quot; as would be useful in performing
   * aberration corrections through the frame.
   */
  public static final ImmutableMap<CelestialFrames, CelestialBodies> FRAME_CENTER_MAP;

  static {
    ImmutableMap.Builder<CelestialFrames, CelestialBodies> builder = ImmutableMap.builder();

    /*
     * All inertial frames have the SSB as their natural center.
     */
    for (CelestialFrames frame : CelestialFrames.values()) {
      if (frame.isInertial()) {
        builder.put(frame, CelestialBodies.SOLAR_SYSTEM_BARYCENTER);
      }
    }

    /*
     * All of the IAU_{BODY} frames have {BODY} as their natural center.
     */
    for (CelestialFrames frame : CelestialFrames.values()) {
      if (frame.name().startsWith("IAU_")) {
        builder.put(frame, CelestialBodies.valueOf(frame.name().replaceFirst("IAU_", "")));
      }
    }

    /*
     * And lastly the custom Earth frames.
     */
    builder.put(CelestialFrames.EARTH_FIXED, CelestialBodies.EARTH);
    builder.put(CelestialFrames.ITRF93, CelestialBodies.EARTH);

    FRAME_CENTER_MAP = builder.build();
  }

}
