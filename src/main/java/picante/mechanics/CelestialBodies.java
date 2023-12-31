package picante.mechanics;

/**
 * This enumeration provides implementations of the EphemerisID interface for all of the built-in
 * (recognized or standard) celestial bodies.
 * <p>
 * At the moment it includes all of the planetary system barycenters, the solar system barycenter,
 * the planets, the moons built into the SPICE Toolkit, as well as a few select smaller bodies. The
 * choice here was a bit arbitrary, and was a specific subset of the bodies built directly into
 * SPICE.
 * </p>
 */
public enum CelestialBodies implements EphemerisID {
  SOLAR_SYSTEM_BARYCENTER, MERCURY_BARYCENTER, VENUS_BARYCENTER, EARTH_BARYCENTER, MARS_BARYCENTER, JUPITER_BARYCENTER, SATURN_BARYCENTER, URANUS_BARYCENTER, NEPTUNE_BARYCENTER, PLUTO_BARYCENTER, SUN, MERCURY, VENUS, EARTH, MOON, MARS, PHOBOS, DEIMOS, JUPITER, IO, EUROPA, GANYMEDE, CALLISTO, AMALTHEA, HIMALIA, ELARA, PASIPHAE, SINOPE, LYSITHEA, CARME, ANANKE, LEDA, THEBE, ADRASTEA, METIS, CALLIRRHOE, THEMISTO, MEGACLITE, TAYGETE, CHALDENE, HARPALYKE, KALYKE, IOCASTE, ERINOME, ISONOE, PRAXIDIKE, AUTONOE, THYONE, HERMIPPE, AITNE, EURYDOME, EUANTHE, EUPORIE, ORTHOSIE, SPONDE, KALE, PASITHEE, HEGEMONE, MNEME, AOEDE, THELXINOE, ARCHE, KALLICHORE, HELIKE, CARPO, EUKELADE, CYLLENE, KORE, HERSE, DIA, SATURN, MIMAS, ENCELADUS, TETHYS, DIONE, RHEA, TITAN, HYPERION, IAPETUS, PHOEBE, JANUS, EPIMETHEUS, HELENE, TELESTO, CALYPSO, ATLAS, PROMETHEUS, PANDORA, PAN, YMIR, PAALIAQ, TARVOS, IJIRAQ, SUTTUNGR, KIVIUQ, MUNDILFARI, ALBIORIX, SKATHI, ERRIAPUS, SIARNAQ, THRYMR, NARVI, METHONE, PALLENE, POLYDEUCES, DAPHNIS, AEGIR, BEBHIONN, BERGELMIR, BESTLA, FARBAUTI, FENRIR, FORNJOT, HATI, HYRROKKIN, KARI, LOGE, SKOLL, SURTUR, ANTHE, JARNSAXA, GREIP, TARQEQ, AEGAEON, URANUS, ARIEL, UMBRIEL, TITANIA, OBERON, MIRANDA, CORDELIA, OPHELIA, BIANCA, CRESSIDA, DESDEMONA, JULIET, PORTIA, ROSALIND, BELINDA, PUCK, CALIBAN, SYCORAX, PROSPERO, SETEBOS, STEPHANO, TRINCULO, FRANCISCO, MARGARET, FERDINAND, PERDITA, MAB, CUPID, NEPTUNE, TRITON, NEREID, NAIAD, THALASSA, DESPINA, GALATEA, LARISSA, PROTEUS, HALIMEDE, PSAMATHE, SAO, LAOMEDEIA, NESO, PLUTO, CHARON, NIX, HYDRA, KERBEROS, STYX, GASPRA, IDA, EROS, BORRELLY, TEMPEL_1, VESTA, ITOKAWA, CERES, PALLAS, LUTETIA, DAVIDA, STEINS, BENNU, A52_EUROPA, RYUGU, ARROKOTH, DIDYMOS_BARYCENTER, DIDYMOS, DIMORPHOS, DONALDJOHANSON, EURYBATES, EURYBATES_BARYCENTER, QUETA, POLYMELE, LEUCUS, ORUS, PATROCLUS_BARYCENTER, PATROCLUS, MENOETIUS;

  @Override
  public String getName() {
    return name();
  }

}
