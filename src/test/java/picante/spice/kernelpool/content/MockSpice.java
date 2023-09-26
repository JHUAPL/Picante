package picante.spice.kernelpool.content;

import java.util.List;
import java.util.Map;
import com.google.common.collect.BiMap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;
import picante.spice.kernelpool.BasicKernelPool;

public class MockSpice {
  private final SpiceInfoHolder infoHolder;

  private MockSpice(BasicKernelPool pool) {
    KernelPoolValidatingRetriever retriever = new KernelPoolValidatingRetriever(pool);
    this.infoHolder =
        new SpiceInfoHolder(framesSetup(), retriever, frameCodeMapSetup(), ephemerisCodeMapSetup());
  }

  public SpiceInfoHolder getInfoHolder() {
    return infoHolder;
  }

  public static class Builder {
    private final BasicKernelPool pool = new BasicKernelPool();

    public MockSpice build() {
      return new MockSpice(pool);
    }

    void addToPool(FrameInfo info, String keyword, String value) {
      String prefix = "FRAME_" + info.getCode() + "_";
      pool.addStrings(prefix + keyword, Lists.newArrayList(value));
    }

    void addToPool(FrameInfo info, String keyword, double value) {
      String prefix = "FRAME_" + info.getCode() + "_";
      pool.addDoubles(prefix + keyword, Lists.newArrayList(value));
    }

    void addToPool(FrameInfo info, String keyword, List<Double> values) {
      String prefix = "FRAME_" + info.getCode() + "_";
      pool.addDoubles(prefix + keyword, values);
    }

    void addIntegersToPool(FrameInfo info, String keyword, List<Integer> values) {
      String prefix = "FRAME_" + info.getCode() + "_";
      pool.addIntegers(prefix + keyword, values);
    }
  }

  private Map<String, Integer> ephemerisCodeMapSetup() {
    return ImmutableMap.of("SUN", 1, "SATURN", 2);
  }

  private Map<String, Integer> frameCodeMapSetup() {
    return ImmutableMap.of("J2000", 1);
  }

  private BiMap<Integer, FrameInfo> framesSetup() {
    return ImmutableBiMap.of(1, new FrameInfo("J2000", 1, FrameType.INERTIAL, 1, 1));
  }
}
