package picante.spice.kernel.tk.pck;

import java.util.Collections;
import java.util.List;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimaps;

public class TextPCK {

  private final List<PCKFrameFunction> functions;
  private final ListMultimap<Integer, Double> radii;

  public TextPCK(List<PCKFrameFunction> functions, ListMultimap<Integer, Double> radii) {
    this.functions = Collections.unmodifiableList(functions);
    this.radii = Multimaps.unmodifiableListMultimap(radii);
  }

  public List<PCKFrameFunction> getFrameFunctions() {
    return functions;
  }

  public ListMultimap<Integer, Double> getRadii() {
    return radii;
  }
}
