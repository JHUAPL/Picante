package picante.spice;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import picante.spice.kernel.KernelInstantiationException;
import picante.spice.kernelpool.KernelPool;
import picante.spice.kernelpool.parser.ParseException;
import picante.spice.kernelpool.parser.TextKernelParser;

/**
 * Read a SPICE metakernel. It does not actually load the kernels. Use the method
 * {@link #getKernelsToLoad()} to pass the list of files to load to the
 * {@link SpiceEnvironmentBuilder}. All keywords except for PATH_SYMBOLS, PATH_VALUES, and
 * KERNELS_TO_LOAD are loaded into the kernel pool accessible with {@link #getKernelPool()}.
 * <p>
 * This class does not rigorously follow all of the SPICE metakernel rules, but it mostly works.
 * Known differences are:
 * <ul>
 * <li>This class has no string length limits, while SPICE has an 80 character string limit, 132
 * character line limit, and 255 character filename limit.</li>
 * <li>This class allows kernel pool variable names to be longer than 32 characters.</li>
 * <li>This class does not support the {@literal @} notation when reading dates.</li>
 * <li>This class supports path name substitutions in any variables ending with _TO_LOAD. SPICE only
 * supports path name substitutions in the KERNELS_TO_LOAD variable.</li>
 * </ul>
 * 
 * There may be metakernels that SPICE can load where this class fails and vice versa. Please report
 * any examples to Hari.Nair@jhuapl.edu.
 * 
 * @author nairah1
 *
 */
public class MetakernelReader {

  private String metakernel;
  private KernelPool kernelPool;
  private List<File> kernelsToLoad;
  private List<String> warnLog;
  private List<String> errLog;

  public MetakernelReader(String metakernel) {
    this.metakernel = metakernel;
    this.errLog = new ArrayList<>();
    this.warnLog = new ArrayList<>();

    List<String> keywordsToRemove;
    keywordsToRemove = new ArrayList<>();
    keywordsToRemove.add("PATH_SYMBOLS");
    keywordsToRemove.add("PATH_VALUES");
    keywordsToRemove.add("KERNELS_TO_LOAD");
    if (new File(metakernel).exists()) {
      load();
      kernelPool.removeKeywords(keywordsToRemove);
    } else {
      errLog.add("Cannot read file " + metakernel);
    }
  }

  /**
   * Return true if there are no errors found (e.g. missing metakernel, mismatched PATH_SYMBOLS and
   * PATH_VALUES)
   * 
   * @return
   */
  public boolean isGood() {
    return errLog.size() == 0;
  }

  /**
   * Return true if this is a badly formed metakernel (e.g. lines longer than 80 characters, missing
   * kernel in list).
   * 
   * @return
   */
  public boolean hasWarnings() {
    return warnLog.size() > 0;
  }

  /**
   * Return the {@link KernelPool}. This includes variables defined in the metakernel, while the
   * {@link SpiceEnvironment} kernel pool does not.
   * 
   * @return
   */
  public KernelPool getKernelPool() {
    return kernelPool;
  }

  /**
   * Return the list of kernels to load in the order specified in the metakernel.
   * 
   * @return
   */
  public List<File> getKernelsToLoad() {
    return kernelsToLoad;
  }

  private void load() {
    kernelsToLoad = new ArrayList<>();
    try {
      TextKernelParser parser = new TextKernelParser();
      kernelPool = new KernelPool(parser.parse(new FileReader(metakernel)));

      Map<String, String> pathMap = new LinkedHashMap<>();
      List<String> pathSymbols = kernelPool.getStrings("PATH_SYMBOLS");
      List<String> pathValues = kernelPool.getStrings("PATH_VALUES");
      /*-
      // PATH_SYMBOLS and PATH_VALUES are not required
      			if (pathSymbols == null)
      				errLog.add(String.format("No PATH_SYMBOLS defined in %s\n", metakernel));
      			if (pathValues == null)
      				errLog.add(String.format("No PATH_VALUES defined in %s\n", metakernel));
      				*/
      if (pathSymbols != null && pathValues != null && pathSymbols.size() != pathValues.size()) {
        errLog.add(String.format("PATH_SYMBOLS has %d entries while PATH_VALUES has %d entries",
            pathSymbols.size(), pathValues.size()));
      }
      if (pathSymbols != null && pathValues != null && pathSymbols.size() == pathValues.size()) {
        for (int i = 0; i < pathSymbols.size(); i++) {
          pathMap.put(pathSymbols.get(i), pathValues.get(i));
        }
      }

      for (String keyword : kernelPool.getKeywords()) {
        if (keyword.length() > 32) {
          warnLog.add(String.format("Kernel variable %s has length %d (SPICE max is 32 characters)",
              keyword, keyword.length()));
        }
        if (keyword.endsWith("_TO_LOAD")) {
          List<String> values = kernelPool.getStrings(keyword);
          List<String> newValues = new ArrayList<>();
          if (values != null) {
            for (String value : values) {

              // SPICE won't read this, but it's okay for crucible
              if (value.length() > 80) {
                warnLog.add(
                    String.format("Badly formed metakernel %s: line length > 80 characters.\n\t%s",
                        metakernel, value));
              }

              for (String symbol : pathMap.keySet()) {
                value = value.replaceAll("\\$" + symbol, pathMap.get(symbol));
              }
              newValues.add(value);
            }
            kernelPool.addStrings(keyword, newValues);
          }
        }
      }

      if (kernelPool.hasKeyword("KERNELS_TO_LOAD")) {
        List<String> kernels = kernelPool.getStrings("KERNELS_TO_LOAD");
        int index = 0;
        while (index < kernels.size()) {
          String path = kernels.get(index);

          // handle continuation character
          if (path.endsWith("+")) {
            path = path.substring(0, path.lastIndexOf("+")) + kernels.get(++index);
          }

          File f = new File(path);
          if (f.exists()) {
            kernelsToLoad.add(f);
          } else {
            warnLog.add("Cannot find kernel " + path);
          }
          index++;
        }
      }
    } catch (FileNotFoundException | ParseException e) {
      e.printStackTrace();
    }
  }

  /**
   * Return the list of errors.
   * 
   * @return
   */
  public List<String> getErrLog() {
    return errLog;
  }

  /**
   * Return the list of warnings.
   * 
   * @return
   */
  public List<String> getWarnLog() {
    return warnLog;
  }

  public static void main(String[] args)
      throws KernelInstantiationException, FileNotFoundException, IOException {

    List<File> kernelPaths = new ArrayList<>();
    KernelPool kernelPool = new KernelPool();

    MetakernelReader mkReader = new MetakernelReader("dart-continued.tm");
    if (mkReader.isGood()) {
      kernelPaths.addAll(mkReader.getKernelsToLoad());
      kernelPool.load(mkReader.getKernelPool());
      if (mkReader.hasWarnings()) {
        for (String s : mkReader.getWarnLog()) {
          System.out.println(s.trim());
        }
      }
    } else {
      for (String s : mkReader.getErrLog()) {
        System.out.println(s.trim());
      }
      System.out.println("Did not load " + mkReader);
    }

    mkReader = new MetakernelReader("dart-long.tm");
    if (mkReader.isGood()) {
      kernelPaths.addAll(mkReader.getKernelsToLoad());
      kernelPool.load(mkReader.getKernelPool());
      if (mkReader.hasWarnings()) {
        for (String s : mkReader.getWarnLog()) {
          System.out.println(s.trim());
        }
      }
    } else {
      for (String s : mkReader.getErrLog()) {
        System.out.println(s.trim());
      }
      System.out.println("Did not load " + mkReader);
    }

    SpiceEnvironmentBuilder builder = new SpiceEnvironmentBuilder();
    for (File f : kernelPaths) {
      if (f.exists()) {
        System.out.println(f.getCanonicalPath());
        builder.forgivingLoad(f.getCanonicalPath(), f);
      }
    }

    SpiceEnvironment env = builder.build();
    kernelPool.load(env.getPool());
    for (String keyword : kernelPool.getKeywords()) {
      System.out.println(keyword);
    }
  }

}
