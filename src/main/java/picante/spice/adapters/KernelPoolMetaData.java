package picante.spice.adapters;

import java.util.Collections;
import java.util.List;
import picante.spice.SpiceMetaData;

public class KernelPoolMetaData extends SpiceMetaData {

  @Override
  public List<String> getComments() {
    return Collections.emptyList();
  }

  @Override
  public String getName() {
    return "ContentDerivedFromKernelPool";
  }

}
