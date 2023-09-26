package picante.mechanics.providers.reference;

import java.util.Collections;
import java.util.List;
import picante.mechanics.FrameProvider;
import picante.mechanics.FrameProviderTest;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;

public class ReferenceEphemerisProviderFrameTest extends FrameProviderTest {

  @Override
  public FrameProvider createProvider(List<? extends FrameTransformFunction> sources) {
    List<PositionVectorFunction> ephemerisSources = Collections.emptyList();

    return new ReferenceEphemerisProvider(ephemerisSources, sources);

  }
}
