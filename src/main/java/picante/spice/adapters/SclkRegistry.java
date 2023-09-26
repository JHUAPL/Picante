package picante.spice.adapters;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import picante.spice.kernel.tk.sclk.EncodedSCLKConverter;

/**
 * Class that connects C-kernel instrument ID codes to the appropriate encoded SCLK conversion
 * implementation.
 * <p>
 * As C-kernels are indexed in encoded SCLK, rather than TDB seconds, the adapters that implement
 * the crucible mechanics package interfaces must be able to convert between these alternate time
 * systems to TDB seconds and back. Instances of this class serve as a registry for these
 * connections.
 * </p>
 * <p>
 * This class provides a specific mechanism for determining which encoded SCLK converter is
 * appropriate for a particular C-kernel ID. First, the map connecting CK integer codes directly to
 * encoded SCLK converters is interrogated. This is done, because it allows users to inject their
 * own converters for ID codes of interest. Then the CK integer code to SCLK integer code map is
 * examined. This map should be populated with frame kernel content first, and then any user
 * requested values should be overlaid. Lastly, the default NAIF algorithm for spacecraft frame
 * codes is used.
 * </p>
 */
public class SclkRegistry {

  /**
   * A map that describes any specific SCLK bindings for a particular C-kernel instrument ID code.
   * Typically these are specified via the CK_<INSTRUMENT_ID>_SCLK entries in the kernel pool.
   */
  private final Map<Integer, EncodedSCLKConverter> ckSpecificMap;

  private final Map<Integer, Integer> ckSclkIdMap;

  /**
   * A map that connects SCLK ID codes to implementations of the encoded SCLK conversion interface.
   * This map is used to handle the default SCLK kernel conversion.
   */
  private final Map<Integer, EncodedSCLKConverter> sclkMap;

  /**
   * Creates a registry of encoded SCLK converters for the frame source adapter factory
   * implementation to support adapting C-kernels to the crucible mechanics interfaces.
   * 
   * @param sclkMap a map connecting SPICE SCLK ID codes to the implementations of the encoded SCLK
   *        converters.
   * 
   * @param ckSpecificMap a map connecting SPICE CK ID codes to the implementations of the encoded
   *        SCLK converters.
   */
  public SclkRegistry(Map<Integer, ? extends EncodedSCLKConverter> sclkMap,
      Map<Integer, Integer> ckSclkIdMap, Map<Integer, EncodedSCLKConverter> ckSpecificMap) {

    /*
     * Make the defensive copy as this class is part of the public API and ensuring immutability of
     * internals is the intention.
     */
    this.sclkMap = Collections.unmodifiableMap(new HashMap<Integer, EncodedSCLKConverter>(sclkMap));
    this.ckSclkIdMap = Collections.unmodifiableMap(new HashMap<Integer, Integer>(ckSclkIdMap));
    this.ckSpecificMap =
        Collections.unmodifiableMap(new HashMap<Integer, EncodedSCLKConverter>(ckSpecificMap));
  }

  /**
   * Obtains an instance of {@link EncodedSCLKConverter} appropriate for the supplied C-kernel
   * instrument ID.
   * 
   * @param instrumentID a SPICE CK instrument ID code
   * 
   * @return a reference to an appropriate implementation of <code>EncodedSclkConverter</code> for
   *         the supplied ID
   * 
   * @throws AdapterInstantiationException if no such converter can be found in the registry
   */
  public EncodedSCLKConverter getSclk(int instrumentID) throws AdapterInstantiationException {

    /*
     * First check to see if the supplied CK integer code to SCLK converter map has an entry for the
     * supplied code.
     */
    if (ckSpecificMap.containsKey(instrumentID)) {
      return ckSpecificMap.get(instrumentID);
    }

    /*
     * If we reach this point then either use the provided ckSclkIdMap (preferred) or attempt a
     * resolution using the SPICE fall back algorithm for spacecraft frames.
     */
    try {
      return fetchSCLK(ckSclkIdMap.containsKey(instrumentID) ? ckSclkIdMap.get(instrumentID)
          : computeDefaultSclkID(instrumentID));
    } catch (SclkRetrievalException e) {
      throw new AdapterInstantiationException(
          "Unable to locate SCLK content for C-kernel ID: " + instrumentID, e);
    }

  }

  /**
   * Retrieves the appropriate SCLK from the internal SCLK map for the specified ID.
   * 
   * @param sclkID the integer ID code for the desired SCLK
   * 
   * @return the converter that is stored in the supplied map.
   * 
   * @throws SclkRetrievalException if the map does not contain a value for the requested ID.
   */
  private EncodedSCLKConverter fetchSCLK(int sclkID) throws SclkRetrievalException {

    if (sclkMap.containsKey(sclkID)) {
      return sclkMap.get(sclkID);
    }

    throw new SclkRetrievalException("Unable to locate SCLK content for ID code: " + sclkID);
  }

  /**
   * Computes the default SCLK ID derived from the C-kernel instrument ID code used by SPICE in the
   * absence of the configuration keyword in the kernel pool.
   * 
   * @param instrumentID the instrument ID of the C-kernel segment in question
   * 
   * @return the default SCLK ID in the event that no explicit instrument ID code binding is found.
   *         (Generally the latter case only happens when a C-kernel instrument ID exceeds -1000).
   * 
   * @throws AdapterInstantiationException if the supplied instrument ID code would not have an
   *         applicable default per SPICE's standard treatment of C-kernel IDs.
   */
  private int computeDefaultSclkID(int instrumentID) throws AdapterInstantiationException {
    if (instrumentID <= -1000) {
      return instrumentID / 1000;
    }
    throw new AdapterInstantiationException("Supplied C-kernel ID: " + instrumentID
        + " is inappropriate for a default SCLK binding " + "per SPICE specification.");

  }

  /**
   * Simple extension of the <code>AdapterInstantiationException</code> that allows an appropriate
   * exception to be generated when a query into the sclkMap fails.
   */
  class SclkRetrievalException extends AdapterInstantiationException {

    /**
     * Default serial version UID.
     */
    private static final long serialVersionUID = 1L;

    public SclkRetrievalException(String message) {
      super(message);
    }

  }

}
