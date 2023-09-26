package picante.spice.kernelpool.content;

import java.util.List;
import java.util.Map;
import picante.spice.kernel.tk.fk.FrameInfo;

public class ParsingUtilities {
  public static int parseEphemeris(String keyword, FrameInfo info, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    Map<String, Integer> ephemerisCodeMap = infoHolder.getEphemerisCodeMap();
    keyword = formatFrameKeyword(keyword, info, retriever);
    if (retriever.getPool().isStringValued(keyword)) {
      String ephemeris = retriever.getString(keyword);
      if (ephemerisCodeMap.containsKey(ephemeris)) {
        return ephemerisCodeMap.get(ephemeris);
      }
      throw new KernelPoolValidationException("Unable to locate code for ephemeris: " + ephemeris);
    }
    if (retriever.getPool().isIntegerValued(keyword)) {
      return retriever.getInteger(keyword);
    }
    throw new KernelPoolValidationException("Unable to parse ephemeris for keyword: " + keyword);
  }

  public static int parseFrame(String keyword, FrameInfo info, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    Map<String, Integer> frameCodeMap = infoHolder.getFrameCodeMap();
    keyword = formatFrameKeyword(keyword, info, retriever);
    if (retriever.getPool().isStringValued(keyword)) {
      String frame = retriever.getString(keyword);
      if (frameCodeMap.containsKey(frame)) {
        return frameCodeMap.get(frame);
      }
      throw new KernelPoolValidationException("Unable to locate code for frame: " + frame);
    }
    if (retriever.getPool().isIntegerValued(keyword)) {
      return retriever.getInteger(keyword);
    }
    throw new KernelPoolValidationException("Unable to parse frame for keyword: " + keyword);
  }

  public static String parseString(String keyword, FrameInfo info, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    String strKeyword = formatFrameKeyword(keyword, info, retriever);
    String str = retriever.getString(strKeyword);
    return str;
  }

  public static SpiceAberrationCorrection parseAbCorr(String keyword, FrameInfo info,
      SpiceInfoHolder infoHolder) throws KernelPoolValidationException {
    SpiceAberrationCorrection abCorr = SpiceAberrationCorrection.NONE;
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    String strKeyword = formatFrameKeyword(keyword, info, retriever);
    if (retriever.containsKeyword(strKeyword)) {
      String abCorrStr = retriever.getString(strKeyword);
      abCorr = SpiceAberrationCorrection.fromString(abCorrStr);
    }
    return abCorr;
  }

  public static double parseDouble(String keyword, FrameInfo info, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    String valKeyword = formatFrameKeyword(keyword, info, retriever);
    double val = retriever.getDouble(valKeyword);
    return val;
  }

  public static int parseInteger(String keyword, FrameInfo info, SpiceInfoHolder infoHolder)
      throws KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    String valKeyword = formatFrameKeyword(keyword, info, retriever);
    int val = retriever.getInteger(valKeyword);
    return val;
  }

  public static List<Double> parseDoubles(String keyword, FrameInfo info,
      SpiceInfoHolder infoHolder) throws KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    String valKeyword = formatFrameKeyword(keyword, info, retriever);
    List<Double> vals = retriever.getDoubles(valKeyword);
    return vals;
  }

  public static List<Integer> parseIntegers(String keyword, FrameInfo info,
      SpiceInfoHolder infoHolder, int expectedLength) throws KernelPoolValidationException {
    KernelPoolValidatingRetriever retriever = infoHolder.getRetriever();
    String valKeyword = formatFrameKeyword(keyword, info, retriever);
    List<Integer> vals = retriever.getIntegersExpectedLength(valKeyword, expectedLength);
    return vals;
  }

  public static String formatFrameKeyword(String keyword, FrameInfo info,
      KernelPoolValidatingRetriever retriever) {
    String name = info.getName();
    int id = info.getCode();
    String formattedKeyword = FKFactory.getFrameKeyword(keyword, id, name, retriever);
    return formattedKeyword;
  }
}
