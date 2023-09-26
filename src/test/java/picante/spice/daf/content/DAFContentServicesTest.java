package picante.spice.daf.content;

import static org.junit.Assert.assertEquals;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import com.google.common.collect.ImmutableList;
import picante.spice.daf.DAF;
import picante.spice.daf.DAFAccessException;
import picante.spice.daf.DAFFactory;
import picante.spice.daf.DAFSegment;

public class DAFContentServicesTest {

  private ArrayDAF increasing;
  private ArrayDAF nondecreasing;

  @Before
  public void setUp() throws Exception {

    double[] iValues = new double[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    double[] ndValues = new double[] {1, 2, 3, 3, 3, 4, 5, 6, 7, 8, 9, 10};

    increasing = new ArrayDAF(iValues);
    nondecreasing = new ArrayDAF(ndValues);

  }

  @Test(expected = DAFAccessException.class)
  public void testLastLessThanSearchIndexConflictException() {
    DAFContentServices.lastLessThanSearch(increasing, 5, 3, 5);
  }

  @Test
  public void testLastLessThanSearchValueSmall() {
    assertEquals(-1, DAFContentServices.lastLessThanSearch(increasing, 3, 6, -1));
    assertEquals(-1, DAFContentServices.lastLessThanSearch(increasing, 0, 9, -1));
    assertEquals(-1, DAFContentServices.lastLessThanSearch(nondecreasing, 2, 8, 2));
    assertEquals(-1, DAFContentServices.lastLessThanSearch(nondecreasing, 2, 4, 3));
  }

  @Test
  public void testLastLessThanSearchValueLarge() {
    assertEquals(3, DAFContentServices.lastLessThanSearch(increasing, 3, 6, 11));
    assertEquals(9, DAFContentServices.lastLessThanSearch(increasing, 0, 9, 11));
    assertEquals(4, DAFContentServices.lastLessThanSearch(nondecreasing, 0, 4, 4));
  }

  @Test
  public void testLastLessThanSearch() {
    assertEquals(3, DAFContentServices.lastLessThanSearch(increasing, 0, 9, 5));
    assertEquals(1, DAFContentServices.lastLessThanSearch(increasing, 7, 9, 10));
    assertEquals(6, DAFContentServices.lastLessThanSearch(nondecreasing, 0, 11, 6));
    assertEquals(2, DAFContentServices.lastLessThanSearch(nondecreasing, 1, 3, 3.5));
    assertEquals(0, DAFContentServices.lastLessThanSearch(nondecreasing, 1, 3, 3));
    assertEquals(1, DAFContentServices.lastLessThanSearch(nondecreasing, 0, 11, 3));
  }

  @Test(expected = DAFAccessException.class)
  public void testLastLessThanOrEqualSearchIndexConflictException() {
    DAFContentServices.lastLessThanOrEqualSearch(increasing, 5, 3, 5);
  }

  @Test
  public void testLastLessThanOrEqualSearchValueSmall() {
    assertEquals(-1, DAFContentServices.lastLessThanOrEqualSearch(increasing, 3, 6, -1));
    assertEquals(-1, DAFContentServices.lastLessThanOrEqualSearch(increasing, 0, 9, -1));
    assertEquals(-1, DAFContentServices.lastLessThanOrEqualSearch(nondecreasing, 2, 8, 2));
    assertEquals(-1, DAFContentServices.lastLessThanOrEqualSearch(nondecreasing, 2, 4, 3));
  }

  @Test
  public void testLastLessThanOrEqualSearchValueLarge() {
    assertEquals(3, DAFContentServices.lastLessThanOrEqualSearch(increasing, 3, 6, 11));
    assertEquals(9, DAFContentServices.lastLessThanOrEqualSearch(increasing, 0, 9, 11));
    assertEquals(4, DAFContentServices.lastLessThanOrEqualSearch(nondecreasing, 0, 4, 4));
  }

  @Test
  public void testLastLessThanOrEqualSearch() {
    assertEquals(4, DAFContentServices.lastLessThanOrEqualSearch(increasing, 0, 9, 5));
    assertEquals(2, DAFContentServices.lastLessThanOrEqualSearch(increasing, 7, 9, 10));
    assertEquals(7, DAFContentServices.lastLessThanOrEqualSearch(nondecreasing, 0, 11, 6));
    assertEquals(2, DAFContentServices.lastLessThanOrEqualSearch(nondecreasing, 1, 3, 3.5));
    assertEquals(2, DAFContentServices.lastLessThanOrEqualSearch(nondecreasing, 1, 3, 3));
    assertEquals(4, DAFContentServices.lastLessThanOrEqualSearch(nondecreasing, 0, 11, 3));
  }

  @Test
  public void testExtractComments() throws Exception {
    DAF daf = DAFFactory.createDAF(DAFContentServicesTest.class.getResourceAsStream("comment.bsp"));

    List<String> comments = DAFContentServices.extractComments(daf, new ArrayList<String>());

    /*
     * Just captured the output from commnt -r into an array of strings.
     */
    String[] expectedText = {"; type2.bsp LOG FILE", "", "; Created 2008-06-04/14:21:19.00.", ";",
        "; BEGIN SPKMERGE COMMANDS", "",
        "LEAPSECONDS_KERNEL   = /Users/turnefs1/Kernels/cassini/lsk/naif0008.tls", "",
        "SPK_KERNEL           = type2.bsp", "  SOURCE_SPK_KERNEL  = native.bsp",
        "    INCLUDE_COMMENTS = NO", "    BODIES           = 399",
        "    BEGIN_TIME       = 2007 APR 04 11:58:55.814",
        "    END_TIME         = 2007 APR 16 11:58:54.814", "", "; END SPKMERGE COMMANDS"};

    List<String> expected = new ArrayList<String>(expectedText.length);
    for (String s : expectedText) {
      expected.add(s);
    }

    assertEquals(expected, comments);
  }

  @Test
  public void testExtractCommentsEmptyReservedRecords() throws Exception {
    DAF daf = DAFFactory.createDAF(DAFContentServicesTest.class.getResourceAsStream("empty.bsp"));
    List<String> comments = DAFContentServices.extractComments(daf, new ArrayList<String>());
    assertEquals(0, comments.size());
  }

  @Test
  public void testCreateCommentsEmptyCommentList() throws Exception {
    assertEquals(0, DAFContentServices.createComments(ImmutableList.<String>of()).length);
  }

  @Test
  public void testCreateCommentsEmptyStringList() throws Exception {
    assertEquals(0, DAFContentServices.createComments(ImmutableList.of("", "", "")).length);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCreateCommentsStringWithNonPrintingCharacters() throws Exception {
    DAFContentServices.createComments(ImmutableList.of("\u0001"));
  }

  @Test
  public void testCreateComments() throws Exception {

    ImmutableList<String> comments =
        ImmutableList.of("Comment line #1.", "And number 2.", "How about 3?");

    byte[] asReservedRecords = DAFContentServices.createComments(comments);


    /*
     * Convert the byte array back into strings for comparison purposes.
     */
    String asString = new String(asReservedRecords, StandardCharsets.ISO_8859_1);

    String[] lines = asString.split("\u0000");

    for (int i = 0; i < comments.size(); i++) {
      assertEquals(comments.get(i), lines[i]);
    }

    assertEquals("\u0004", lines[lines.length - 1]);

  }

}


@SuppressWarnings("unused")
class ArrayDAF implements DAFSegment {

  private double[] array;

  public ArrayDAF(double[] values) {
    this.array = values;
  }

  @Override
  public void get(int index, double[] buffer, int offset, int length) {
    System.arraycopy(array, index, buffer, offset, length);
  }

  @Override
  public double getDoubleComponent(int index) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getIntComponent(int index) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getLength() {
    return array.length;
  }

  @Override
  public int getND() {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getNI() {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getName() {
    throw new UnsupportedOperationException();
  }

}
