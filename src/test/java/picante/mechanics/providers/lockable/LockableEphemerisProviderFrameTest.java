package picante.mechanics.providers.lockable;

import java.util.Collections;
import java.util.List;
import picante.mechanics.FrameProvider;
import picante.mechanics.FrameProviderTest;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;

public class LockableEphemerisProviderFrameTest extends FrameProviderTest {

  @Override
  public FrameProvider createProvider(List<? extends FrameTransformFunction> sources) {

    List<PositionVectorFunction> ephemerisSources = Collections.emptyList();

    return new LockableEphemerisProvider(ephemerisSources, sources, LockType.FUNCTION);

  }
}
