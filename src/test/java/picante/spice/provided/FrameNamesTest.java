package picante.spice.provided;

import static org.junit.Assert.assertTrue;
import java.util.Map;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.Sets;
import picante.mechanics.CelestialFrames;
import picante.mechanics.FrameID;
import picante.spice.kernel.tk.fk.FrameInfo;
import picante.spice.kernel.tk.fk.FrameType;

public class FrameNamesTest {

  private FrameNames names;

  @Before
  public void setUp() throws Exception {
    this.names = new FrameNames();
  }

  @Test
  public void testGetStandardBindings() {
    /*
     * Check that all the frames defined in the CelestialFrames are present in the standard binding.
     * It's not worth fussing over the individual integer bindings, as software generated them in
     * the first place.
     */
    Map<Integer, FrameID> map = names.getStandardBindings();

    for (CelestialFrames frame : CelestialFrames.values()) {
      assertTrue(frame.getName() + " not found.", map.containsValue(frame));
    }
  }

  @Test
  public void testGetMap() {

    /*
     * Make certain that any of the FrameInfo presented in the standard map do not have multiple
     * class/classID entries.
     */
    Map<Integer, FrameInfo> map = names.getMap();

    Set<ClassClassID> set = Sets.newHashSet();

    for (FrameInfo info : map.values()) {
      assertTrue(info.getName(), set.add(new ClassClassID(info.getType(), info.getClassID())));
    }

  }

  private class ClassClassID {

    private final FrameType type;
    private final int id;

    public ClassClassID(FrameType type, int id) {
      super();
      this.type = type;
      this.id = id;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + getOuterType().hashCode();
      result = prime * result + id;
      result = prime * result + ((type == null) ? 0 : type.hashCode());
      return result;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      ClassClassID other = (ClassClassID) obj;
      if (!getOuterType().equals(other.getOuterType())) {
        return false;
      }
      if (id != other.id) {
        return false;
      }
      if (type != other.type) {
        return false;
      }
      return true;
    }

    private FrameNamesTest getOuterType() {
      return FrameNamesTest.this;
    }

  }

}
