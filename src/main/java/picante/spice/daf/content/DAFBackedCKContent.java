package picante.spice.daf.content;

import picante.spice.daf.DAF;
import picante.spice.daf.DAFSegment;
import picante.spice.kernel.ck.CKInstantiationException;
import picante.spice.kernel.ck.CKSegment;

public class DAFBackedCKContent
    extends DAFBasedKernelContent<CKSegment, CKInstantiationException, CKSegmentFactory> {

  /**
   * Creates a CK file from the supplied DAF.
   * 
   * @param daf the DAF containing the CK data
   * @param factory a segment creation factory
   * 
   * @throws CKInstantiationException if anything fails in the instantiation of the CK file
   */
  public DAFBackedCKContent(DAF daf, CKSegmentFactory factory) throws CKInstantiationException {
    super(daf, factory);
  }

  /**
   * {@inheritDoc}
   * 
   * Simply checks that the ID word is either NAIF/DAF or DAF/CK.
   */
  @Override
  void verifyContent(String idWord) throws CKInstantiationException {
    if ((!idWord.equals("NAIF/DAF")) && (!idWord.equals("DAF/CK"))) {
      throw new CKInstantiationException("Supporting DAF ID word is [" + idWord
          + "] which does not indicate a DAF with CK content.");
    }
  }

  /**
   * {@inheritDoc}
   * 
   * Simply checks to see that ND and NI are 2 and 4 respectively.
   */
  @Override
  void verifySegment(DAFSegment segment) throws CKInstantiationException {

    if (segment.getND() != 2) {
      throw new CKInstantiationException("The number of double precision metadata components "
          + "in the underlying DAF [" + segment.getND() + "] should be 2.");
    }

    if (segment.getNI() != 4) {
      throw new CKInstantiationException("The number of integer metadata components in the "
          + "underlying DAF [" + segment.getNI() + "] should be 4.");
    }

  }

}
