package picante.spice.kernelpool.content;

import java.util.HashMap;
import java.util.Map;
import picante.mechanics.providers.aberrated.AberrationCorrection;

public enum SpiceAberrationCorrection {
  NONE("NONE", AberrationCorrection.NONE, false),
  //
  LT("LT", AberrationCorrection.LT, false),
  //
  LT_S("LT+S", AberrationCorrection.LT_S, false),
  //
  CN("CN", AberrationCorrection.LT, true),
  //
  CN_S("CN+S", AberrationCorrection.LT_S, true),
  //
  XLT("XLT", AberrationCorrection.XLT, false),
  //
  XLT_S("XLT+S", AberrationCorrection.XLT_S, false),
  //
  XCN("XCN", AberrationCorrection.XLT, true),
  //
  XCN_S("XCN+S", AberrationCorrection.XLT_S, true);

  private final String string;
  private final AberrationCorrection abCorr;
  private final boolean triple;

  @Override
  public String toString() {
    return string;
  }

  public AberrationCorrection getAbCorr() {
    return abCorr;
  }

  public boolean isTriple() {
    return triple;
  }

  private SpiceAberrationCorrection(String string, AberrationCorrection abCorr, boolean triple) {
    this.string = string;
    this.abCorr = abCorr;
    this.triple = triple;
  }

  private static final Map<String, SpiceAberrationCorrection> stringToEnum = new HashMap<>();
  static {
    for (SpiceAberrationCorrection abCorr : values()) {
      stringToEnum.put(abCorr.toString(), abCorr);
    }
  }

  public static SpiceAberrationCorrection fromString(String abCorrStr)
      throws KernelPoolValidationException {
    abCorrStr = abCorrStr.trim();
    SpiceAberrationCorrection abCorr = stringToEnum.get(abCorrStr);
    if (abCorr == null) {
      throw new KernelPoolValidationException("Invalid Spice Aberration Correction: " + abCorrStr);
    }
    return abCorr;
  }
}
