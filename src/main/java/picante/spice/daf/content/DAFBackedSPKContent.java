package picante.spice.daf.content;

import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.spk.SPKInstantiationException;
import picante.spice.kernel.spk.SPKSegment;

public class DAFBackedSPKContent
    extends DAFBasedKernelContent<SPKSegment, SPKInstantiationException, SPKSegmentFactory> {

  /**
   * Creates an SPK file from the supplied DAF.
   * 
   * @param daf the DAF containing the SPK data.
   * @param factory a segment creation factory
   * 
   * @throws SPKInstantiationException if anything fails with the creation of the SPK file
   * 
   * @see SPKSegmentFactory
   */
  public DAFBackedSPKContent(DAF daf, SPKSegmentFactory factory) throws SPKInstantiationException {
    super(daf, factory);
  }

  /**
   * {@inheritDoc}
   * 
   * Simply checks that the ID word is either NAIF/DAF or DAF/CK.
   */
  @Override
  void verifyContent(String idWord) throws SPKInstantiationException {
    if ((!idWord.equals("NAIF/DAF")) && (!idWord.equals("DAF/SPK"))) {
      throw new SPKInstantiationException("Supporting DAF ID word is [" + idWord
          + "] which does not indicate a DAF with SPK content.");
    }
  }

  /**
   * {@inheritDoc}
   * 
   * Simply checks that ND and NI are 2 and 4 respectively.
   */
  @Override
  void verifySegment(DAFSegment segment) throws SPKInstantiationException {
    if (segment.getND() != 2) {
      throw new SPKInstantiationException("The number of double precision metadata components "
          + "in the underlying DAF [" + segment.getND() + "] should be 2.");
    }

    if (segment.getNI() != 4) {
      throw new SPKInstantiationException("The number of integer metadata components in the "
          + "underlying DAF [" + segment.getNI() + "] should be 4.");
    }
  }

}
