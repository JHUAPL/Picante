package picante.spice.kernelpool.content;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class SpiceAberrationCorrectionTest {

  @Test
  public void testFormat() throws KernelPoolValidationException {
    /*
     * Just checking that the fromString static method does return the proper abcorr, and that the
     * tostring method returns the desired string
     */
    checkString("NONE");
    checkString("LT");
    checkString("LT+S");
    checkString("CN");
    checkString("CN+S");
    checkString("XLT");
    checkString("XLT+S");
    checkString("XCN");
    checkString("XCN+S");
  }

  private void checkString(String abCorrStr) throws KernelPoolValidationException {
    assertEquals(SpiceAberrationCorrection.fromString(abCorrStr).toString(), abCorrStr);
  }

}
