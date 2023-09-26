package picante.spice.kernelpool.content;

import java.util.Map;

import com.google.common.collect.BiMap;
import picante.spice.kernel.tk.fk.FrameInfo;

public class SpiceInfoHolder {
  private final BiMap<Integer, FrameInfo> frames;
  private final KernelPoolValidatingRetriever retriever;
  private final Map<String, Integer> frameCodeMap;
  private final Map<String, Integer> ephemerisCodeMap;

  public BiMap<Integer, FrameInfo> getFrames() {
    return frames;
  }

  public KernelPoolValidatingRetriever getRetriever() {
    return retriever;
  }

  public Map<String, Integer> getFrameCodeMap() {
    return frameCodeMap;
  }

  public Map<String, Integer> getEphemerisCodeMap() {
    return ephemerisCodeMap;
  }

  public SpiceInfoHolder(BiMap<Integer, FrameInfo> frames, KernelPoolValidatingRetriever retriever,
      Map<String, Integer> frameCodeMap, Map<String, Integer> ephemerisCodeMap) {
    super();
    this.frames = frames;
    this.retriever = retriever;
    this.frameCodeMap = frameCodeMap;
    this.ephemerisCodeMap = ephemerisCodeMap;
  }


}
