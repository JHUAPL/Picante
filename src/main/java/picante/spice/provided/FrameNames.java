package picante.spice.provided;

import java.util.Map;

import com.google.common.collect.BiMap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.Maps;
import picante.exceptions.BugException;
import picante.mechanics.CelestialFrames;
import picante.mechanics.FrameID;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;

public class FrameNames {

  public static final String FROM_TOOLKIT_VERSION = "N0067";

  private ImmutableBiMap<Integer, FrameInfo> map;
  private ImmutableBiMap<Integer, FrameID> standardBindings;

  public FrameNames() {

    /*
     * Create the map from integers to frame information. This is necessarily a 1:1 and onto
     * mapping. The frame information class canonicalizes the frame name for us.
     */
    ImmutableBiMap.Builder<Integer, FrameInfo> builder = ImmutableBiMap.builder();

    builder.put(1, new FrameInfo("J2000", 1, FrameType.getTypeForClassInteger(1), 1, 0));
    builder.put(2, new FrameInfo("B1950", 2, FrameType.getTypeForClassInteger(1), 2, 0));
    builder.put(3, new FrameInfo("FK4", 3, FrameType.getTypeForClassInteger(1), 3, 0));
    builder.put(4, new FrameInfo("DE-118", 4, FrameType.getTypeForClassInteger(1), 4, 0));
    builder.put(5, new FrameInfo("DE-96", 5, FrameType.getTypeForClassInteger(1), 5, 0));
    builder.put(6, new FrameInfo("DE-102", 6, FrameType.getTypeForClassInteger(1), 6, 0));
    builder.put(7, new FrameInfo("DE-108", 7, FrameType.getTypeForClassInteger(1), 7, 0));
    builder.put(8, new FrameInfo("DE-111", 8, FrameType.getTypeForClassInteger(1), 8, 0));
    builder.put(9, new FrameInfo("DE-114", 9, FrameType.getTypeForClassInteger(1), 9, 0));
    builder.put(10, new FrameInfo("DE-122", 10, FrameType.getTypeForClassInteger(1), 10, 0));
    builder.put(11, new FrameInfo("DE-125", 11, FrameType.getTypeForClassInteger(1), 11, 0));
    builder.put(12, new FrameInfo("DE-130", 12, FrameType.getTypeForClassInteger(1), 12, 0));
    builder.put(13, new FrameInfo("GALACTIC", 13, FrameType.getTypeForClassInteger(1), 13, 0));
    builder.put(14, new FrameInfo("DE-200", 14, FrameType.getTypeForClassInteger(1), 14, 0));
    builder.put(15, new FrameInfo("DE-202", 15, FrameType.getTypeForClassInteger(1), 15, 0));
    builder.put(16, new FrameInfo("MARSIAU", 16, FrameType.getTypeForClassInteger(1), 16, 0));
    builder.put(17, new FrameInfo("ECLIPJ2000", 17, FrameType.getTypeForClassInteger(1), 17, 0));
    builder.put(18, new FrameInfo("ECLIPB1950", 18, FrameType.getTypeForClassInteger(1), 18, 0));
    builder.put(19, new FrameInfo("DE-140", 19, FrameType.getTypeForClassInteger(1), 19, 0));
    builder.put(20, new FrameInfo("DE-142", 20, FrameType.getTypeForClassInteger(1), 20, 0));
    builder.put(21, new FrameInfo("DE-143", 21, FrameType.getTypeForClassInteger(1), 21, 0));
    builder.put(10001,
        new FrameInfo("IAU_MERCURY_BARYCENTER", 10001, FrameType.getTypeForClassInteger(2), 1, 1));
    builder.put(10002,
        new FrameInfo("IAU_VENUS_BARYCENTER", 10002, FrameType.getTypeForClassInteger(2), 2, 2));
    builder.put(10003,
        new FrameInfo("IAU_EARTH_BARYCENTER", 10003, FrameType.getTypeForClassInteger(2), 3, 3));
    builder.put(10004,
        new FrameInfo("IAU_MARS_BARYCENTER", 10004, FrameType.getTypeForClassInteger(2), 4, 4));
    builder.put(10005,
        new FrameInfo("IAU_JUPITER_BARYCENTER", 10005, FrameType.getTypeForClassInteger(2), 5, 5));
    builder.put(10006,
        new FrameInfo("IAU_SATURN_BARYCENTER", 10006, FrameType.getTypeForClassInteger(2), 6, 6));
    builder.put(10007,
        new FrameInfo("IAU_URANUS_BARYCENTER", 10007, FrameType.getTypeForClassInteger(2), 7, 7));
    builder.put(10008,
        new FrameInfo("IAU_NEPTUNE_BARYCENTER", 10008, FrameType.getTypeForClassInteger(2), 8, 8));
    builder.put(10009,
        new FrameInfo("IAU_PLUTO_BARYCENTER", 10009, FrameType.getTypeForClassInteger(2), 9, 9));
    builder.put(10010,
        new FrameInfo("IAU_SUN", 10010, FrameType.getTypeForClassInteger(2), 10, 10));
    builder.put(10011,
        new FrameInfo("IAU_MERCURY", 10011, FrameType.getTypeForClassInteger(2), 199, 199));
    builder.put(10012,
        new FrameInfo("IAU_VENUS", 10012, FrameType.getTypeForClassInteger(2), 299, 299));
    builder.put(10013,
        new FrameInfo("IAU_EARTH", 10013, FrameType.getTypeForClassInteger(2), 399, 399));
    builder.put(10014,
        new FrameInfo("IAU_MARS", 10014, FrameType.getTypeForClassInteger(2), 499, 499));
    builder.put(10015,
        new FrameInfo("IAU_JUPITER", 10015, FrameType.getTypeForClassInteger(2), 599, 599));
    builder.put(10016,
        new FrameInfo("IAU_SATURN", 10016, FrameType.getTypeForClassInteger(2), 699, 699));
    builder.put(10017,
        new FrameInfo("IAU_URANUS", 10017, FrameType.getTypeForClassInteger(2), 799, 799));
    builder.put(10018,
        new FrameInfo("IAU_NEPTUNE", 10018, FrameType.getTypeForClassInteger(2), 899, 899));
    builder.put(10019,
        new FrameInfo("IAU_PLUTO", 10019, FrameType.getTypeForClassInteger(2), 999, 999));
    builder.put(10020,
        new FrameInfo("IAU_MOON", 10020, FrameType.getTypeForClassInteger(2), 301, 301));
    builder.put(10021,
        new FrameInfo("IAU_PHOBOS", 10021, FrameType.getTypeForClassInteger(2), 401, 401));
    builder.put(10022,
        new FrameInfo("IAU_DEIMOS", 10022, FrameType.getTypeForClassInteger(2), 402, 402));
    builder.put(10023,
        new FrameInfo("IAU_IO", 10023, FrameType.getTypeForClassInteger(2), 501, 501));
    builder.put(10024,
        new FrameInfo("IAU_EUROPA", 10024, FrameType.getTypeForClassInteger(2), 502, 502));
    builder.put(10025,
        new FrameInfo("IAU_GANYMEDE", 10025, FrameType.getTypeForClassInteger(2), 503, 503));
    builder.put(10026,
        new FrameInfo("IAU_CALLISTO", 10026, FrameType.getTypeForClassInteger(2), 504, 504));
    builder.put(10027,
        new FrameInfo("IAU_AMALTHEA", 10027, FrameType.getTypeForClassInteger(2), 505, 505));
    builder.put(10028,
        new FrameInfo("IAU_HIMALIA", 10028, FrameType.getTypeForClassInteger(2), 506, 506));
    builder.put(10029,
        new FrameInfo("IAU_ELARA", 10029, FrameType.getTypeForClassInteger(2), 507, 507));
    builder.put(10030,
        new FrameInfo("IAU_PASIPHAE", 10030, FrameType.getTypeForClassInteger(2), 508, 508));
    builder.put(10031,
        new FrameInfo("IAU_SINOPE", 10031, FrameType.getTypeForClassInteger(2), 509, 509));
    builder.put(10032,
        new FrameInfo("IAU_LYSITHEA", 10032, FrameType.getTypeForClassInteger(2), 510, 510));
    builder.put(10033,
        new FrameInfo("IAU_CARME", 10033, FrameType.getTypeForClassInteger(2), 511, 511));
    builder.put(10034,
        new FrameInfo("IAU_ANANKE", 10034, FrameType.getTypeForClassInteger(2), 512, 512));
    builder.put(10035,
        new FrameInfo("IAU_LEDA", 10035, FrameType.getTypeForClassInteger(2), 513, 513));
    builder.put(10036,
        new FrameInfo("IAU_THEBE", 10036, FrameType.getTypeForClassInteger(2), 514, 514));
    builder.put(10037,
        new FrameInfo("IAU_ADRASTEA", 10037, FrameType.getTypeForClassInteger(2), 515, 515));
    builder.put(10038,
        new FrameInfo("IAU_METIS", 10038, FrameType.getTypeForClassInteger(2), 516, 516));
    builder.put(10039,
        new FrameInfo("IAU_MIMAS", 10039, FrameType.getTypeForClassInteger(2), 601, 601));
    builder.put(10040,
        new FrameInfo("IAU_ENCELADUS", 10040, FrameType.getTypeForClassInteger(2), 602, 602));
    builder.put(10041,
        new FrameInfo("IAU_TETHYS", 10041, FrameType.getTypeForClassInteger(2), 603, 603));
    builder.put(10042,
        new FrameInfo("IAU_DIONE", 10042, FrameType.getTypeForClassInteger(2), 604, 604));
    builder.put(10043,
        new FrameInfo("IAU_RHEA", 10043, FrameType.getTypeForClassInteger(2), 605, 605));
    builder.put(10044,
        new FrameInfo("IAU_TITAN", 10044, FrameType.getTypeForClassInteger(2), 606, 606));
    builder.put(10045,
        new FrameInfo("IAU_HYPERION", 10045, FrameType.getTypeForClassInteger(2), 607, 607));
    builder.put(10046,
        new FrameInfo("IAU_IAPETUS", 10046, FrameType.getTypeForClassInteger(2), 608, 608));
    builder.put(10047,
        new FrameInfo("IAU_PHOEBE", 10047, FrameType.getTypeForClassInteger(2), 609, 609));
    builder.put(10048,
        new FrameInfo("IAU_JANUS", 10048, FrameType.getTypeForClassInteger(2), 610, 610));
    builder.put(10049,
        new FrameInfo("IAU_EPIMETHEUS", 10049, FrameType.getTypeForClassInteger(2), 611, 611));
    builder.put(10050,
        new FrameInfo("IAU_HELENE", 10050, FrameType.getTypeForClassInteger(2), 612, 612));
    builder.put(10051,
        new FrameInfo("IAU_TELESTO", 10051, FrameType.getTypeForClassInteger(2), 613, 613));
    builder.put(10052,
        new FrameInfo("IAU_CALYPSO", 10052, FrameType.getTypeForClassInteger(2), 614, 614));
    builder.put(10053,
        new FrameInfo("IAU_ATLAS", 10053, FrameType.getTypeForClassInteger(2), 615, 615));
    builder.put(10054,
        new FrameInfo("IAU_PROMETHEUS", 10054, FrameType.getTypeForClassInteger(2), 616, 616));
    builder.put(10055,
        new FrameInfo("IAU_PANDORA", 10055, FrameType.getTypeForClassInteger(2), 617, 617));
    builder.put(10056,
        new FrameInfo("IAU_ARIEL", 10056, FrameType.getTypeForClassInteger(2), 701, 701));
    builder.put(10057,
        new FrameInfo("IAU_UMBRIEL", 10057, FrameType.getTypeForClassInteger(2), 702, 702));
    builder.put(10058,
        new FrameInfo("IAU_TITANIA", 10058, FrameType.getTypeForClassInteger(2), 703, 703));
    builder.put(10059,
        new FrameInfo("IAU_OBERON", 10059, FrameType.getTypeForClassInteger(2), 704, 704));
    builder.put(10060,
        new FrameInfo("IAU_MIRANDA", 10060, FrameType.getTypeForClassInteger(2), 705, 705));
    builder.put(10061,
        new FrameInfo("IAU_CORDELIA", 10061, FrameType.getTypeForClassInteger(2), 706, 706));
    builder.put(10062,
        new FrameInfo("IAU_OPHELIA", 10062, FrameType.getTypeForClassInteger(2), 707, 707));
    builder.put(10063,
        new FrameInfo("IAU_BIANCA", 10063, FrameType.getTypeForClassInteger(2), 708, 708));
    builder.put(10064,
        new FrameInfo("IAU_CRESSIDA", 10064, FrameType.getTypeForClassInteger(2), 709, 709));
    builder.put(10065,
        new FrameInfo("IAU_DESDEMONA", 10065, FrameType.getTypeForClassInteger(2), 710, 710));
    builder.put(10066,
        new FrameInfo("IAU_JULIET", 10066, FrameType.getTypeForClassInteger(2), 711, 711));
    builder.put(10067,
        new FrameInfo("IAU_PORTIA", 10067, FrameType.getTypeForClassInteger(2), 712, 712));
    builder.put(10068,
        new FrameInfo("IAU_ROSALIND", 10068, FrameType.getTypeForClassInteger(2), 713, 713));
    builder.put(10069,
        new FrameInfo("IAU_BELINDA", 10069, FrameType.getTypeForClassInteger(2), 714, 714));
    builder.put(10070,
        new FrameInfo("IAU_PUCK", 10070, FrameType.getTypeForClassInteger(2), 715, 715));
    builder.put(10071,
        new FrameInfo("IAU_TRITON", 10071, FrameType.getTypeForClassInteger(2), 801, 801));
    builder.put(10072,
        new FrameInfo("IAU_NEREID", 10072, FrameType.getTypeForClassInteger(2), 802, 802));
    builder.put(10073,
        new FrameInfo("IAU_NAIAD", 10073, FrameType.getTypeForClassInteger(2), 803, 803));
    builder.put(10074,
        new FrameInfo("IAU_THALASSA", 10074, FrameType.getTypeForClassInteger(2), 804, 804));
    builder.put(10075,
        new FrameInfo("IAU_DESPINA", 10075, FrameType.getTypeForClassInteger(2), 805, 805));
    builder.put(10076,
        new FrameInfo("IAU_GALATEA", 10076, FrameType.getTypeForClassInteger(2), 806, 806));
    builder.put(10077,
        new FrameInfo("IAU_LARISSA", 10077, FrameType.getTypeForClassInteger(2), 807, 807));
    builder.put(10078,
        new FrameInfo("IAU_PROTEUS", 10078, FrameType.getTypeForClassInteger(2), 808, 808));
    builder.put(10079,
        new FrameInfo("IAU_CHARON", 10079, FrameType.getTypeForClassInteger(2), 901, 901));
    builder.put(10081,
        new FrameInfo("EARTH_FIXED", 10081, FrameType.getTypeForClassInteger(4), 10081, 399));
    builder.put(10082,
        new FrameInfo("IAU_PAN", 10082, FrameType.getTypeForClassInteger(2), 618, 618));
    builder.put(10083,
        new FrameInfo("IAU_GASPRA", 10083, FrameType.getTypeForClassInteger(2), 9511010, 9511010));
    builder.put(10084,
        new FrameInfo("IAU_IDA", 10084, FrameType.getTypeForClassInteger(2), 2431010, 2431010));
    builder.put(10085,
        new FrameInfo("IAU_EROS", 10085, FrameType.getTypeForClassInteger(2), 2000433, 2000433));
    builder.put(10086,
        new FrameInfo("IAU_CALLIRRHOE", 10086, FrameType.getTypeForClassInteger(2), 517, 517));
    builder.put(10087,
        new FrameInfo("IAU_THEMISTO", 10087, FrameType.getTypeForClassInteger(2), 518, 518));
    builder.put(10088,
        new FrameInfo("IAU_MEGACLITE", 10088, FrameType.getTypeForClassInteger(2), 519, 519));
    builder.put(10089,
        new FrameInfo("IAU_TAYGETE", 10089, FrameType.getTypeForClassInteger(2), 520, 520));
    builder.put(10090,
        new FrameInfo("IAU_CHALDENE", 10090, FrameType.getTypeForClassInteger(2), 521, 521));
    builder.put(10091,
        new FrameInfo("IAU_HARPALYKE", 10091, FrameType.getTypeForClassInteger(2), 522, 522));
    builder.put(10092,
        new FrameInfo("IAU_KALYKE", 10092, FrameType.getTypeForClassInteger(2), 523, 523));
    builder.put(10093,
        new FrameInfo("IAU_IOCASTE", 10093, FrameType.getTypeForClassInteger(2), 524, 524));
    builder.put(10094,
        new FrameInfo("IAU_ERINOME", 10094, FrameType.getTypeForClassInteger(2), 525, 525));
    builder.put(10095,
        new FrameInfo("IAU_ISONOE", 10095, FrameType.getTypeForClassInteger(2), 526, 526));
    builder.put(10096,
        new FrameInfo("IAU_PRAXIDIKE", 10096, FrameType.getTypeForClassInteger(2), 527, 527));
    builder.put(10097, new FrameInfo("IAU_BORRELLY", 10097, FrameType.getTypeForClassInteger(2),
        1000005, 1000005));
    builder.put(10098, new FrameInfo("IAU_TEMPEL_1", 10098, FrameType.getTypeForClassInteger(2),
        1000093, 1000093));
    builder.put(10099,
        new FrameInfo("IAU_VESTA", 10099, FrameType.getTypeForClassInteger(2), 2000004, 2000004));
    builder.put(10100,
        new FrameInfo("IAU_ITOKAWA", 10100, FrameType.getTypeForClassInteger(2), 2025143, 2025143));
    builder.put(10101,
        new FrameInfo("IAU_CERES", 10101, FrameType.getTypeForClassInteger(2), 2000001, 2000001));
    builder.put(10102,
        new FrameInfo("IAU_PALLAS", 10102, FrameType.getTypeForClassInteger(2), 2000002, 2000002));
    builder.put(10103,
        new FrameInfo("IAU_LUTETIA", 10103, FrameType.getTypeForClassInteger(2), 2000021, 2000021));
    builder.put(10104,
        new FrameInfo("IAU_DAVIDA", 10104, FrameType.getTypeForClassInteger(2), 2000511, 2000511));
    builder.put(10105,
        new FrameInfo("IAU_STEINS", 10105, FrameType.getTypeForClassInteger(2), 2002867, 2002867));
    builder.put(10106,
        new FrameInfo("IAU_BENNU", 10106, FrameType.getTypeForClassInteger(2), 2101955, 2101955));
    builder.put(10107, new FrameInfo("IAU_52_EUROPA", 10107, FrameType.getTypeForClassInteger(2),
        2000052, 2000052));
    builder.put(10108,
        new FrameInfo("IAU_NIX", 10108, FrameType.getTypeForClassInteger(2), 902, 902));
    builder.put(10109,
        new FrameInfo("IAU_HYDRA", 10109, FrameType.getTypeForClassInteger(2), 903, 903));
    builder.put(10110,
        new FrameInfo("IAU_RYUGU", 10110, FrameType.getTypeForClassInteger(2), 2162173, 2162173));
    builder.put(10111, new FrameInfo("IAU_ARROKOTH", 10111, FrameType.getTypeForClassInteger(2),
        2486958, 2486958));
    builder.put(10112, new FrameInfo("IAU_DIDYMOS_BARYCENTER", 10112,
        FrameType.getTypeForClassInteger(2), 20065803, 20065803));
    builder.put(10113, new FrameInfo("IAU_DIDYMOS", 10113, FrameType.getTypeForClassInteger(2),
        920065803, 920065803));
    builder.put(10114, new FrameInfo("IAU_DIMORPHOS", 10114, FrameType.getTypeForClassInteger(2),
        120065803, 120065803));
    builder.put(10115, new FrameInfo("IAU_DONALDJOHANSON", 10115,
        FrameType.getTypeForClassInteger(2), 20052246, 20052246));
    builder.put(10116, new FrameInfo("IAU_EURYBATES", 10116, FrameType.getTypeForClassInteger(2),
        920003548, 920003548));
    builder.put(10117, new FrameInfo("IAU_EURYBATES_BARYCENTER", 10117,
        FrameType.getTypeForClassInteger(2), 20003548, 20003548));
    builder.put(10118, new FrameInfo("IAU_QUETA", 10118, FrameType.getTypeForClassInteger(2),
        120003548, 120003548));
    builder.put(10119, new FrameInfo("IAU_POLYMELE", 10119, FrameType.getTypeForClassInteger(2),
        20015094, 20015094));
    builder.put(10120, new FrameInfo("IAU_LEUCUS", 10120, FrameType.getTypeForClassInteger(2),
        20011351, 20011351));
    builder.put(10121,
        new FrameInfo("IAU_ORUS", 10121, FrameType.getTypeForClassInteger(2), 20021900, 20021900));
    builder.put(10122, new FrameInfo("IAU_PATROCLUS_BARYCENTER", 10122,
        FrameType.getTypeForClassInteger(2), 20000617, 20000617));
    builder.put(10123, new FrameInfo("IAU_PATROCLUS", 10123, FrameType.getTypeForClassInteger(2),
        920000617, 920000617));
    builder.put(10124, new FrameInfo("IAU_MENOETIUS", 10124, FrameType.getTypeForClassInteger(2),
        120000617, 120000617));
    builder.put(13000,
        new FrameInfo("ITRF93", 13000, FrameType.getTypeForClassInteger(2), 3000, 399));

    try {
      this.map = builder.build();
      this.standardBindings = createStandardBindings();
    } catch (IllegalArgumentException e) {
      throw new BugException(
          "Unable to create built-in frame names " + "and mappings.  This is clearly a bug.", e);
    }

  }

  private ImmutableBiMap<Integer, FrameID> createStandardBindings() {

    ImmutableBiMap.Builder<Integer, FrameID> resultBuilder = ImmutableBiMap.builder();

    Map<String, Integer> strings = Maps.newHashMap();
    for (FrameInfo info : map.values()) {
      strings.put(info.getName(), info.getCode());
    }

    for (CelestialFrames frame : CelestialFrames.values()) {
      if (strings.containsKey(frame.getName())) {
        resultBuilder.put(strings.get(frame.getName()), frame);
      } else {
        /*
         * Handle the DE-<NUMBER> cases separately, as '-' is not a supported character in
         * enumerations.
         */
        if (frame.getName().startsWith("DE")) {
          resultBuilder.put(strings.get(frame.getName().replaceFirst("DE", "DE-")), frame);
        } else if ( frame.equals(CelestialFrames.IAU_A52_EUROPA)) {
          resultBuilder.put(strings.get("IAU_52_EUROPA"), frame);
        }
      }
    }

    return resultBuilder.build();
  }

  /**
   * Retrieves the standard bindings for the frame ID code, <b>not</b> the class ID code associated
   * with the frame implementation.
   * 
   * @return a mapping from integral frame ID codes to the standard, built-in frame IDs.
   * 
   * @see CelestialFrames
   */
  public Map<Integer, FrameID> getStandardBindings() {
    return standardBindings;
  }

  public BiMap<Integer, FrameInfo> getMap() {
    return map;
  }

}
