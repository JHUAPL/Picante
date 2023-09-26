package picante.spice.kernelpool.parser.time;

import java.io.StringReader;

/**
 * A parser that converts formal calendar strings to seconds past the J2000 reference epoch.
 * <p>
 * To use it, one simply invokes it as follows:
 * 
 * <pre>
 * double secondsPastJ2000 = parse("2000-JAN-01 12:00");
 * </pre>
 * 
 * This single method takes as input a string containing the time and converts it to seconds past
 * the J2000 reference epoch.
 * <p>
 * This parser attempts to implement the NAIF/SPICELIB time parser, TPARSE. There are several
 * divergent aspects of this parser from that implemented by NAIF. First, the parsing of time
 * strings in SPICE is broken down into a 6 stage process:
 * <ol>
 * <li>First pass, attempt to match input string against a set of known patterns.</li>
 * <li>Remove '-' and '/' from the input string, attempt again to match it against the same set of
 * known patterns.</li>
 * <li>Attempt to match any sequence i:i:i:(i|n) against the Day:Hour:Minute:Second
 * interpretation.</li>
 * <li>Remove colons from the input stream.</li>
 * <li>Attempt to interpret the remaining tokens in the stream according to other, somewhat
 * non-standard patterns.</li>
 * <li>Lastly, check for the presence of remaining numeric tokens that are not-interpreted.</li>
 * </ol>
 * 
 * This implementation of the time parser performs the first of these six steps and stops.
 * Experiments with existing NAIF text kernels indicate that this should be sufficient for
 * compliance with existing kernels. But, it is a sticky point that should be considered to complete
 * it.
 * <p>
 * Another deviation from NAIF's kernel parser is the strictYear() non-terminal. NAIF requires years
 * to be interpreted as two digits next to an era specification or quote character, but explicitly
 * disallows 3 digits. This parser relaxes that requirement, and allows: 100(AD) and '100 to be
 * interpreted as strictYear() specifications.
 * <p>
 * The final deviation of NAIF's text kernel parser has to do with date specifications that supply
 * fractional days, instead of the usual hour, minute, second specification. Due to a feature in
 * NAIF's underlying parser code, the fractional component of the days are truncated. This parser
 * does not suffer from this limitation, and correctly handles the fractional portion of the day
 * time.
 * <p>
 * This parser interprets three kinds of time strings: Julian dates, ISO time formats, and general
 * calendar formats. The patterns that it interprets are as follows:
 * 
 * <pre>
 *      Y-i-it......... YmD             i/i/ii:i:n..... mDYHMS
 *      Y-i-iti........ YmDH            i/i/ii:n....... mDYHM
 *      Y-i-iti:i...... YmDHM           i/i/ii:n....... mDYHM
 *      Y-i-iti:i:i.... YmDHMS          i:i:ii-i-Y..... HMSmDY
 *      Y-i-iti:i:n.... YmDHMS          i:i:ii/i/Y..... HMSmDY
 *      Y-i-iti:n...... YmDHM           i:i:ii/i/i..... HMSmDY
 *      Y-i-itn........ YmDH            i:i:iimY....... HMSDmY
 *      Y-i/........... Yy              i:i:imiY....... HMSmDY
 *      Y-i/i:i........ YyHM            i:i:ni-i-Y..... HMSmDY
 *      Y-i/i:i:i...... YyHMS           i:i:ni/i/Y..... HMSmDY
 *      Y-i/i:i:n...... YyHMS           i:i:ni/i/i..... HMSmDY
 *      Y-i/i:n........ YyHM            i:i:nimY....... HMSDmY
 *      Y-id........... Yy              i:i:nmiY....... HMSmDY
 *      Y-idi:i........ YyHM            i:ii-i-Y....... HMmDY
 *      Y-idi:i:i...... YyHMS           i:ii/i/Y....... HMmDY
 *      Y-idi:i:n...... YyHMS           i:ii/i/i....... HMmDY
 *      Y-idi:n........ YyHM            i:iimY......... HMDmY
 *      Y-it........... Yy              i:imiY......... HMmDY
 *      Y-iti.......... YyH             i:ni-i-Y....... HMmDY
 *      Y-iti:i........ YyHM            i:ni/i/Y....... HMmDY
 *      Y-iti:i:i...... YyHMS           i:ni/i/i....... HMmDY
 *      Y-iti:i:n...... YyHMS           i:nimY......... HMDmY
 *      Y-iti:n........ YyHM            i:nmiY......... HMmDY
 *      Y-itn.......... YyH             iYd............ yY
 *      Yid............ Yy              iYdi:i......... yYHM
 *      Yidi:i......... YyHM            iYdi:i:i....... yYHMS
 *      Yidi:i:i....... YyHMS           iYdi:i:n....... yYHMS
 *      Yidi:i:n....... YyHMS           iYdi:n......... yYHM
 *      Yidi:n......... YyHM            iiY............ mDY
 *      Yii............ YmD             iiYi........... mDYH
 *      Yiii........... YmDH            iiYi:i......... mDYHM
 *      Yiii:i......... YmDHM           iiYi:i:i....... mDYHMS
 *      Yiii:i:i....... YmDHMS          iiYi:i:n....... mDYHMS
 *      Yiii:i:n....... YmDHMS          iiYi:n......... mDYHM
 *      Yiii:n......... YmDHM           iiYn........... mDYH
 *      Yiiii.......... YmDHM           iid............ Yy
 *      Yiiiii......... YmDHMS          iidi:i......... YyHM
 *      Yiiiin......... YmDHMS          iidi:i:i....... YyHMS
 *      Yiiin.......... YmDHM           iidi:i:n....... YyHMS
 *      Yiin........... YmDH            iidi:n......... YyHM
 *      Yim............ YDm             iim............ YDm
 *      Yimi........... YDmH            iimi........... YDmH
 *      Yimi:i......... YDmHM           iimi:i......... YDmHM
 *      Yimi:i:i....... YDmHMS          iimi:i:i....... YDmHMS
 *      Yimi:i:n....... YDmHMS          iimi:i:n....... YDmHMS
 *      Yimi:n......... YDmHM           iimi:n......... YDmHM
 *      Yimn........... YDmH            iimii.......... YDmHM
 *      Yin............ YmD             iimiii......... YDmHMS
 *      Ymi............ YmD             iimiin......... YDmHMS
 *      Ymii........... YmDH            iimin.......... YDmHM
 *      Ymii:i......... YmDHM           iimn........... YDmH
 *      Ymii:i:i....... YmDHMS          imY............ DmY
 *      Ymii:i:n....... YmDHMS          imYi........... DmYH
 *      Ymii:n......... YmDHM           imYi:i......... DmYHM
 *      Ymin........... YmDH            imYi:i:i....... DmYHMS
 *      Ymn............ YmD             imYi:i:n....... DmYHMS
 *      Ynm............ YDm             imYi:n......... DmYHM
 *      i-Y/........... yY              imYn........... DmYH
 *      i-Y/i:i........ yYHM            imi............ YmD
 *      i-Y/i:i:i...... yYHMS           imi:i:iY....... DmHMSY
 *      i-Y/i:i:n...... yYHMS           imi:i:nY....... DmHMSY
 *      i-Y/i:n........ yYHM            imi:iY......... DmHMY
 *      i-Yd........... yY              imi:nY......... DmHMY
 *      i-Ydi:i........ yYHM            imii........... YmDH
 *      i-Ydi:i:i...... yYHMS           imii:i......... YmDHM
 *      i-Ydi:i:n...... yYHMS           imii:i:i....... YmDHMS
 *      i-Ydi:n........ yYHM            imii:i:n....... YmDHMS
 *      i-i-Y.......... mDY             imii:n......... YmDHM
 *      i-i-Yi:i....... mDYHM           imiii.......... YmDHM
 *      i-i-Yi:i:i..... mDYHMS          imiiii......... YmDHMS
 *      i-i-Yi:i:n..... mDYHMS          imiiin......... YmDHMS
 *      i-i-Yi:n....... mDYHM           imiin.......... YmDHM
 *      i-i-it......... YmD             imin........... YmDH
 *      i-i-iti........ YmDH            imn............ YmD
 *      i-i-iti:i...... YmDHM           inY............ mDY
 *      i-i-iti:i:i.... YmDHMS          inm............ YDm
 *      i-i-iti:i:n.... YmDHMS          miY............ mDY
 *      i-i-iti:n...... YmDHM           miYi........... mDYH
 *      i-i-itn........ YmDH            miYi:i......... mDYHM
 *      i-i/i:i........ YyHM            miYi:i:i....... mDYHMS
 *      i-i/i:i:i...... YyHMS           miYi:i:n....... mDYHMS
 *      i-i/i:i:n...... YyHMS           miYi:n......... mDYHM
 *      i-i/i:n........ YyHM            miYn........... mDYH
 *      i-idi:i........ YyHM            mii............ mDY
 *      i-idi:i:i...... YyHMS           mii:i:iY....... mDHMSY
 *      i-idi:i:n...... YyHMS           mii:i:nY....... mDHMSY
 *      i-idi:n........ YyHM            mii:iY......... mDHMY
 *      i-it........... Yy              mii:nY......... mDHMY
 *      i-iti.......... YyH             miii........... mDYH
 *      i-iti:i........ YyHM            miii:i......... mDYHM
 *      i-iti:i:i...... YyHMS           miii:i:i....... mDYHMS
 *      i-iti:i:n...... YyHMS           miii:i:n....... mDYHMS
 *      i-iti:n........ YyHM            miii:n......... mDYHM
 *      i-itn.......... YyH             miiii.......... mDYHM
 *      i/i/Y.......... mDY             miiiii......... mDYHMS
 *      i/i/Y/i:n...... mDYHM           miiiin......... mDYHMS
 *      i/i/Yi:i....... mDYHM           miiin.......... mDYHM
 *      i/i/Yi:i:i..... mDYHMS          miin........... mDYH
 *      i/i/Yi:i:n..... mDYHMS          mnY............ mDY
 *      i/i/i.......... mDY             mni............ mDY
 *      i/i/ii:i....... mDYHM           nmY............ DmY
 *      i/i/ii:i:i..... mDYHMS 
 *
 *  where:
 *                         Y  ---  Year
 *                         m  ---  Month
 *                         D  ---  Day of Month
 *                         y  ---  Day of Year
 *                         H  ---  Hour
 *                         M  ---  Minute
 *                         S  ---  Second
 * </pre>
 */
public class TimeParser {

  private final Parser parser = new Parser(new StringReader(""));
  private final TParser tparser = new TParser(new StringReader(""));

  /**
   * Parse a time string.
   *
   * <p>
   * NOTE: to be used only by the kernel pool parser. For general use, look at
   * {@link #tparse(String)}.
   *
   * @param time A time string to be parsed
   * @return seconds past the J2000 epoch
   * @throws ParseException
   */
  public double parse(String time) throws ParseException {
    return parser.parse(time);
  }

  /**
   * Parse a time string.
   * 
   * @param time A time string to be parsed
   * @return seconds past the J2000 epoch
   * @throws ParseException
   */
  public double tparse(String time) throws ParseException {
    return tparser.parse(time);
  }

}
