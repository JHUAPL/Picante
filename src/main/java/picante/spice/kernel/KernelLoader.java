package picante.spice.kernel;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.io.ByteSource;
import com.google.common.io.Files;
import picante.exceptions.BugException;
import picante.spice.daf.DAF;
import picante.spice.daf.bytebuffer.ByteBufferDAF;
import picante.spice.daf.content.CKSegmentFactory;
import picante.spice.daf.content.DAFBackedCKContent;
import picante.spice.daf.content.DAFBackedPCKContent;
import picante.spice.daf.content.DAFBackedSPKContent;
import picante.spice.daf.content.DAFContentServices;
import picante.spice.daf.content.PCKSegmentFactory;
import picante.spice.daf.content.SPKSegmentFactory;
import picante.spice.kernel.ck.CK;
import picante.spice.kernel.pck.PCK;
import picante.spice.kernel.spk.SPK;
import picante.spice.kernel.tk.TextKernel;
import picante.spice.kernelpool.parser.ParseException;
import picante.spice.kernelpool.parser.TextKernelParser;

/**
 * Convenience class that provides methods to construct {@link Kernel} content from various data
 * input sources.
 * <p>
 * There are inherent limitations with the utilization of this convenience loader that may be
 * platform dependent. In general files are mapped into byte buffers via the
 * {@link FileChannel#map(MapMode, long, long)} method. As this is using platform specific code
 * bundled with the JVM it is unclear if this will work in all cases.
 * </p>
 * <p>
 * Further, there are limits on the sizes of files passed into this API. All SPICE kernels text or
 * otherwise should be less than 2GB in size.
 * </p>
 * <p>
 * One other point worth mentioning, if for some strange reason you have a text kernel that you are
 * loading into the system that begins with the markers that indicate a DAF (DAF/SPK, DAF/CK,
 * DAF/PCK, NAIF/DAF), then this will likely confuse this loader into believing it has a DAF based
 * kernel. This will ultimately generate some sort of exception from the DAF loader, but will not
 * default back to a text kernel. This is by design.
 * </p>
 */
public class KernelLoader {

  /**
   * Size of the buffer used per pass to read the contents of a stream into memory fully.
   */
  private static final int readBufferSize = 10240;

  /**
   * Matcher used to split the contents of a DAF based ID word.
   */
  private final Matcher dafIdSplitter = Pattern.compile("(.*)/(.*) *").matcher("");

  /**
   * The text kernel parser to utilize in this loader.
   */
  private final TextKernelParser parser = new TextKernelParser();

  /**
   * Creates a SPICE kernel from the contents of the supplied file.
   * <p>
   * The name of the kernel, available from {@link Kernel#getName()}, will be set to the absolute
   * path of the supplied file. Namely via the {@link File#getAbsolutePath()} method.
   * 
   * @param file the file to load
   * 
   * @return a newly created {@link Kernel}
   * 
   * @throws KernelInstantiationException if something fails in the instantiation of the kernel from
   *         the contents of the file
   * @throws FileNotFoundException if the supplied file does not exist on the system
   * @throws IOException if an IO failure occurs when reading the contents of the supplied file
   */
  public Kernel load(File file)
      throws KernelInstantiationException, FileNotFoundException, IOException {
    return load(file.getAbsolutePath(), Files.asByteSource(file));
  }

  /**
   * Creates a SPICE kernel from the contents of the supplied stream.
   * <p>
   * Note: the contents of the supplied stream will be read completely into memory. This may or may
   * not be appropriate, but it is necessary as SPICE kernels frequently require random access.
   * </p>
   * <p>
   * Note: if the supplied stream is a FileInputStream, then the channel will be retrieved and used
   * to obtain a memory mapped byte buffer instead of reading the stream into memory.
   * </p>
   * 
   * @param name the name to supply to return in the {@link Kernel#getName()} method.
   * @param stream the stream with the contents of the kernel to load
   * 
   * @return a newly created Kernel
   * 
   * @throws KernelInstantiationException if something fails in the instantiation of the kernel from
   *         the contents of the file
   * @throws IOException if an IO failure occurs when reading the contents of the supplied stream
   */
  public Kernel load(String name, ByteSource source)
      throws KernelInstantiationException, IOException {

    InputStream stream = source.openStream();

    if (stream instanceof FileInputStream) {
      FileChannel channel = ((FileInputStream) stream).getChannel();
      return load(name, channel.map(MapMode.READ_ONLY, 0, channel.size()));
    }

    /*
     * If we reach here, then we have to read the entire stream contents into memory because it
     * could be a DAF.
     */
    ByteBuffer buffer = loadStreamFully(stream);
    return load(name, buffer);
  }

  /**
   * Loads a kernel with the given name from the contents of the supplied byte buffer.
   * 
   * @param name the name to supply to the kernel constructor
   * @param buffer the buffer containing the contents of the kernel
   * 
   * @return a newly created kernel from the supplied buffer
   * 
   * @throws KernelInstantiationException if anything goes wrong with the creation of the kernel
   * 
   */
  Kernel load(String name, ByteBuffer buffer) throws KernelInstantiationException {

    /*
     * Set the position to the head of the byte buffer, this is where the id word should be.
     */
    buffer.position(0);

    /*
     * Read the first 8 bytes of the buffer that may contain the id word.
     */
    byte[] idWord = new byte[8];
    buffer.get(idWord);

    if (isDAF(idWord)) {
      return createKernelFromDAF(name, new ByteBufferDAF(buffer));
    }

    return createTextKernel(name, buffer);
  }

  /**
   * Determine the DAF type and process it into the appropriate list of kernels.
   * 
   * @param daf a DAF of interest.
   * 
   * @throws KernelInstantiationException if the processing fails for any reason.
   */
  Kernel createKernelFromDAF(String name, DAF daf) throws KernelInstantiationException {

    KernelType type = DAFContentServices.identifyDAF(daf);

    if (type == null) {
      throw new BugException("Unsupported DAF based kernel type. ID word was: " + daf.getID());
    }

    switch (type) {

      case CK:
        return new CK(name, new DAFBackedCKContent(daf, CKSegmentFactory.VALIDATING));
      case SPK:
        return new SPK(name, new DAFBackedSPKContent(daf, SPKSegmentFactory.VALIDATING));
      case PCK:
        return new PCK(name, new DAFBackedPCKContent(daf, PCKSegmentFactory.VALIDATING));

      default:
        throw new BugException("Unsupported DAF based kernel type: " + type);
    }

  }

  /**
   * Given a sequence of 8 bytes from the head of a file or stream, determine if the content is from
   * a DAF.
   * <p>
   * DAF files begin with &quot;NAIF/DAF&quot; or in later versions of the toolkit a template:
   * &quot;DAF/XXXX&quot; where XXXX is replaced with SPK, CK, PCK, or any of the other types NAIF
   * may implement. If the type identifier is less than 4 bytes, it should be space padded.
   * </p>
   * 
   * @param idword first 8 bytes of content from a candidate file.
   * 
   * @return true, if these bytes suggest the content is derived from a DAF.
   */
  boolean isDAF(byte[] idword) {

    String id;

    try {
      id = new String(idword, "ISO-8859-1");
    } catch (UnsupportedEncodingException e) {
      throw new BugException(
          "ISO-8859-1 is an invalid character encoding," + " are you sure this is a real JVM?", e);
    }

    dafIdSplitter.reset(id);

    /*
     * This can only be an ID from a DAF if the splitter successfully matches the text.
     */
    if (dafIdSplitter.matches()) {

      String part1 = dafIdSplitter.group(1);
      String part2 = dafIdSplitter.group(2);

      /*
       * If the idword is: NAIF/DAF or anything of the ilk: DAF/, then it is likely a DAF. Otherwise
       * it must be another type of kernel.
       */
      return (part1.equalsIgnoreCase("NAIF") && part2.equalsIgnoreCase("DAF"))
          || (part1.equalsIgnoreCase("DAF"));

    }

    return false;
  }

  /**
   * Convenience IO method that loads the contents of the attached stream into a byte buffer.
   * 
   * @param istream the stream to read into memory
   * @return a byte buffer with the contents of stream
   * 
   * @throws IOException if reading from the stream fails for any reason
   */
  ByteBuffer loadStreamFully(InputStream istream) throws IOException {

    byte[] buffer = new byte[readBufferSize];
    int bytesRead;
    ByteArrayOutputStream byteStream = new ByteArrayOutputStream();

    /*
     * Read the contents of istream and write them to byteStream.
     */
    bytesRead = istream.read(buffer);

    while (bytesRead > 0) {
      byteStream.write(buffer, 0, bytesRead);
      bytesRead = istream.read(buffer);
    }

    return ByteBuffer.wrap(byteStream.toByteArray());
  }

  /**
   * Load the text kernel associated with the supplied buffer.
   * <p>
   * Note: the created text kernel contains empty comments.
   * </p>
   * 
   * @param identifier the identifier associated with the buffer
   * @param buffer the buffer containing the text kernel content to parse
   * 
   * @throws KernelInstantiationException if parsing the content in buffer fails for any reason
   */
  TextKernel createTextKernel(String name, final ByteBuffer buffer)
      throws KernelInstantiationException {

    /*
     * Set the position to the head of the buffer, as we are going to read the entire contents.
     */
    buffer.position(0);

    try {
      return (new TextKernel(name,
          parser.parse(new ByteBufferReader(buffer, Charset.forName("ISO-8859-1")))));
    } catch (IllegalCharsetNameException e) {
      throw new BugException(
          "ISO-8859-1 charset not supported. " + "Are you sure this is a real JVM?", e);
    } catch (UnsupportedCharsetException e) {
      throw new BugException(
          "ISO-8859-1 charset not supported. " + "Are you sure this is a real JVM?", e);
    } catch (ParseException e) {
      throw new KernelInstantiationException("Parsing text kernel: " + name + " failed.", e);
    }
  }

}
