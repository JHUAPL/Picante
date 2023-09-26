package picante.spice;

import java.util.List;
import picante.mechanics.SourceMetaData;

public abstract class SpiceMetaData implements SourceMetaData {

  public abstract List<String> getComments();

}
