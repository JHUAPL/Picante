package picante.spice.kernel.tk.fk.dynamic;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import com.google.common.collect.Lists;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.spice.SpiceEnvironment;
import picante.spice.SpiceEnvironmentBuilder;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.parser.ParseException;
import picante.spice.kernelpool.parser.TextKernelParser;

public class SpiceEnvironmentSetup {

  public static SpiceEnvironment createDefault()
      throws ParseException, KernelInstantiationException, IOException {
    List<String> metaKernels = Lists.newArrayList();
    List<String> kernels = Lists.newArrayList();
    kernels.add("src/test/resources/picante/spice/naif0012.tls");
    kernels.add("src/test/resources/picante/spice/kernel/tk/fk/dynamic/pck00010.tpc");
    kernels.add("src/test/resources/picante/spice/kernel/tk/fk/dynamic/crucible-test.bsp");
    kernels.add("src/test/resources/picante/spice/kernel/tk/fk/dynamic/custom.tf");

    return create(metaKernels, kernels, Lists.newArrayList(DynFrames.values()),
        Lists.newArrayList());
  }

  public static SpiceEnvironment create(List<String> metaKernels, List<String> kernels,
      List<FrameID> frames, List<EphemerisID> ephemerides)
      throws ParseException, KernelInstantiationException, IOException {
    List<String> allKernels = Lists.newArrayList();

    allKernels.addAll(getKernelsFromMetaKernels(metaKernels));
    allKernels.addAll(kernels);

    SpiceEnvironmentBuilder seb = new SpiceEnvironmentBuilder();
    // seb.setIgnoreFaultyFrames(true);
    for (String kernel : allKernels) {
      seb.load(kernel, new File(kernel));
    }

    for (FrameID frame : frames) {
      seb.bindFrameID(frame.getName(), frame);
    }
    for (EphemerisID ephemeris : ephemerides) {
      seb.bindEphemerisID(ephemeris.getName(), ephemeris);
    }
    SpiceEnvironment se = seb.build();
    return se;
  }

  public static List<String> getKernelsFromMetaKernels(List<String> metaKernelList)
      throws FileNotFoundException, ParseException {
    List<String> kernelList = new ArrayList<>();
    for (String metaKernel : metaKernelList) {
      kernelList.addAll(getKernelStrings(metaKernel));
    }
    return kernelList;
  }

  public static List<String> getKernelStrings(InputStream metaKernel) throws ParseException {
    return getKernelStrings(new InputStreamReader(metaKernel));
  }

  public static List<String> getKernelStrings(String metaKernel)
      throws ParseException, FileNotFoundException {
    return getKernelStrings(new InputStreamReader(new FileInputStream(new File(metaKernel))));
  }

  private static List<String> getKernelStrings(InputStreamReader metaKernelReader)
      throws ParseException {
    TextKernelParser parser = new TextKernelParser();
    BasicKernelPool kernelPool = parser.parse(metaKernelReader);
    List<String> kernelList = new ArrayList<>();
    List<String> symList = kernelPool.getStrings("PATH_SYMBOLS");
    List<String> valList = kernelPool.getStrings("PATH_VALUES");
    List<String> kernList = kernelPool.getStrings("KERNELS_TO_LOAD");
    for (String str : kernList) {
      for (int j = 0; j < symList.size(); j++) {
        String replace = "\\$" + symList.get(j);
        str = str.replaceAll(replace, valList.get(j));
      }
      kernelList.add(str);
    }
    return kernelList;
  }

  public static enum DynFrames implements FrameID {
    SPP_HCI("SPP_HCI", true),

    SPP_HEEQ("SPP_HEEQ", false),

    SPP_MSO("SPP_MSO", false),

    SPP_HGI("SPP_HGI", false),

    SPP_HGSPEC("SPP_HGSPEC", false),

    MEANECLIPTIC("MEANECLIPTIC", false),

    EQUATDATE("EQUATDATE", false),

    TRUEEQDATE("TRUEEQDATE", false);

    private final String name;
    private final boolean isInertial;

    private DynFrames(String name, boolean isInertial) {
      this.name = name;
      this.isInertial = isInertial;
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    public boolean isInertial() {
      return isInertial;
    }
  }
}
