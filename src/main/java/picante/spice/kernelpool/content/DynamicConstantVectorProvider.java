package picante.spice.kernelpool.content;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import picante.math.coords.CoordConverters;
import picante.math.coords.LatitudinalVector;
import picante.math.coords.RaDecVector;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.dynamic.DynamicConstantVectorFunction;
import picante.spice.kernelpool.content.DynamicVectorProvider.VectorID;

enum DynamicConstantVectorProvider {
  RECTANGULAR("RECTANGULAR") {
    @Override
    public DynamicConstantVectorFunction createDynamicConstantVectorFunction(VectorID vectorID,
        FrameInfo info, int relativeCode, int frameCode, SpiceInfoHolder infoHolder)
        throws KernelPoolValidationException {
      List<Double> components = ParsingUtilities
          .parseDoubles("FRAME_%s_" + vectorID.getPrefix() + "_VECTOR", info, infoHolder);
      UnwritableVectorIJK vector =
          new UnwritableVectorIJK(components.get(0), components.get(1), components.get(2));
      return new DynamicConstantVectorFunction(vector, frameCode, relativeCode);
    }
  },
  LATITUDINAL("LATITUDINAL") {
    @Override
    public DynamicConstantVectorFunction createDynamicConstantVectorFunction(VectorID vectorID,
        FrameInfo info, int relativeCode, int frameCode, SpiceInfoHolder infoHolder)
        throws KernelPoolValidationException {
      String unitsStr = ParsingUtilities.parseString("FRAME_%s_" + vectorID.getPrefix() + "_UNITS",
          info, infoHolder);
      DynamicUnits units = DynamicUnits.fromString(unitsStr);
      double longitude = ParsingUtilities
          .parseDouble("FRAME_%s_" + vectorID.getPrefix() + "_LONGITUDE", info, infoHolder);
      double latitude = ParsingUtilities
          .parseDouble("FRAME_%s_" + vectorID.getPrefix() + "_LATITUDE", info, infoHolder);
      longitude = units.convertToRadians(longitude);
      latitude = units.convertToRadians(latitude);

      LatitudinalVector latVec = new LatitudinalVector(1, latitude, longitude);
      UnwritableVectorIJK vector = CoordConverters.convert(latVec);
      return new DynamicConstantVectorFunction(vector, frameCode, relativeCode);
    }
  },
  RA_DEC("RA/DEC") {
    @Override
    public DynamicConstantVectorFunction createDynamicConstantVectorFunction(VectorID vectorID,
        FrameInfo info, int relativeCode, int frameCode, SpiceInfoHolder infoHolder)
        throws KernelPoolValidationException {

      String unitsStr = ParsingUtilities.parseString("FRAME_%s_" + vectorID.getPrefix() + "_UNITS",
          info, infoHolder);
      DynamicUnits units = DynamicUnits.fromString(unitsStr);
      double ra = ParsingUtilities.parseDouble("FRAME_%s_" + vectorID.getPrefix() + "_RA", info,
          infoHolder);
      double dec = ParsingUtilities.parseDouble("FRAME_%s_" + vectorID.getPrefix() + "_DEC", info,
          infoHolder);
      ra = units.convertToRadians(ra);
      dec = units.convertToRadians(dec);

      RaDecVector raDecVec = new RaDecVector(1, ra, dec);
      UnwritableVectorIJK vector = CoordConverters.convert(raDecVec);
      return new DynamicConstantVectorFunction(vector, frameCode, relativeCode);
    }
  };

  private final String name;

  @Override
  public String toString() {
    return name;
  }

  private DynamicConstantVectorProvider(String name) {
    this.name = name;
  }

  public static DynamicConstantVectorFunction createFunction(VectorID vectorID, FrameInfo info,
      int relativeCode, int frameCode, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException {
    String providerStr = ParsingUtilities.parseString("FRAME_%s_" + vectorID.getPrefix() + "_SPEC",
        info, infoHolder);
    DynamicConstantVectorProvider provider = DynamicConstantVectorProvider.fromString(providerStr);
    DynamicConstantVectorFunction function = provider.createDynamicConstantVectorFunction(vectorID,
        info, relativeCode, frameCode, infoHolder);
    return function;
  }

  public abstract DynamicConstantVectorFunction createDynamicConstantVectorFunction(
      VectorID vectorID, FrameInfo info, int relativeCode, int frameCode,
      SpiceInfoHolder infoHolder) throws KernelPoolValidationException;

  static Map<String, DynamicConstantVectorProvider> mapStringToSpec = new HashMap<>();
  static {
    for (DynamicConstantVectorProvider spec : DynamicConstantVectorProvider.values()) {
      mapStringToSpec.put(spec.toString(), spec);
    }
  }

  public static DynamicConstantVectorProvider fromString(String string)
      throws KernelPoolValidationException {
    DynamicConstantVectorProvider provider = mapStringToSpec.get(string);
    if (provider == null) {
      throw new KernelPoolValidationException("Invalid Constant Vector Definition: " + string);
    }
    return provider;
  }
}
