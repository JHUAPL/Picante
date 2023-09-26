/* Parser.java */
/* Generated By:JavaCC: Do not edit this line. Parser.java */
package picante.spice.kernelpool.parser;

import java.util.List;
import java.util.ArrayList;
import picante.spice.kernelpool.BasicKernelPool;
import picante.spice.kernelpool.parser.time.TimeParser;

class Parser implements ParserConstants {

   private final TimeParser timeParser = new TimeParser();

   /**     * The parser action to be utilized by the current instance.    * There need only be a single one.    */
   private final ParserAction action = new ParserAction();

   private double processDate(Token t) throws picante.spice.kernelpool.parser.time.ParseException {
      /*       * Strip the leading '@' off the token's image before sending       * it on to the parser.        */
      return timeParser.parse(t.image.substring(1,t.image.length()));
   }

   private double processDouble(Token t) {

      /*       *  The NAIF text kernel format supports FORTRAN style numeric       *  specifications.  This includes exponent characters that are       *  'D' or 'd', in addition to the usual C-style 'e' or 'E'.        *  Translate any occurrences of this unusual exponent        *  specification to 'e' so parseDouble() will function properly.       */
      String s = t.image.replaceAll("d|D","e");

      /*       *  See if this token is PI.       */
      if ( s.equalsIgnoreCase("PI") | s.equalsIgnoreCase("+PI") ) {
         return Math.PI;
      }

      if ( s.equalsIgnoreCase("-PI") ) {
         return -Math.PI;
      }

      return Double.parseDouble(s);
   }

   private String processQuotedString(Token t) {
      String result = t.image;

      /*       *  Remove the leading and trailing "'", as they are not part       *  of the string content we wish to return.       */
      result = result.substring(1,result.length()-1);

      /*       *  Replace any occurrences of the doubled quote "''" with a        *  a single quote "'".       */
      result = result.replaceAll( "''", "'" );

      return result;
   }

   /**    * Private assistance class that captures the current parser's    * action as it is built up.    */
   private static class ParserAction {
      String keyword;
      boolean append;
      boolean isStringValued;
      List<Double> doubles = new ArrayList<Double>();
      List<String> strings = new ArrayList<String>();

      void clearLists() {
         doubles.clear();
         strings.clear();
      }

      void execute(BasicKernelPool pool) {

         if ( append ) {
            if (isStringValued) {
               pool.appendStrings(keyword, strings);
            } else {
               pool.appendDoubles(keyword, doubles);
            }
         } else {
            if (isStringValued) {
               pool.addStrings(keyword, strings);
            } else {
               pool.addDoubles(keyword, doubles);
            }
         }

      }

   }

/** * Parse the contents of the configured stream into a newly created * basic kernel pool. */
  final public BasicKernelPool parse() throws ParseException {BasicKernelPool pool = new BasicKernelPool();
    try {
      label_1:
      while (true) {
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case KEYWORD_PLUS_EQUALS:
        case KEYWORD:{
          ;
          break;
          }
        default:
          jj_la1[0] = jj_gen;
          break label_1;
        }
        keyword(action);
action.clearLists();
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case DATE:
        case NUMBER:
        case QUOTED_STRING:{
          values(action);
          break;
          }
        case LEFT_PAREN:{
          listValues(action);
          break;
          }
        default:
          jj_la1[1] = jj_gen;
          jj_consume_token(-1);
          throw new ParseException();
        }
action.execute(pool);
      }
      switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
      case LONE_BEGINTEXT:{
        jj_consume_token(LONE_BEGINTEXT);
        break;
        }
      default:
        jj_la1[2] = jj_gen;
        ;
      }
      jj_consume_token(0);
{if ("" != null) return pool;}
    } catch (NumberFormatException e) {
{if (true) throw new ParseException("Unable to parse double: " + e.getMessage());}
    } catch (picante.spice.kernelpool.parser.time.ParseException e) {
{if (true) throw new ParseException("Unable to parse date: " + e.getMessage() );}
    }
    throw new Error("Missing return statement in function");
}

  final private void keyword(ParserAction buffer) throws ParseException {Token t;
    switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
    case KEYWORD_PLUS_EQUALS:{
      t = jj_consume_token(KEYWORD_PLUS_EQUALS);
buffer.keyword = t.image.substring(0,t.image.length()-2);
       buffer.append=true;
      break;
      }
    case KEYWORD:{
      t = jj_consume_token(KEYWORD);
      switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
      case EQUALS:{
        jj_consume_token(EQUALS);
buffer.append=false;
        break;
        }
      case PLUS_EQUALS:{
        jj_consume_token(PLUS_EQUALS);
buffer.append=true;
        break;
        }
      default:
        jj_la1[3] = jj_gen;
        jj_consume_token(-1);
        throw new ParseException();
      }
buffer.keyword=t.image;
      break;
      }
    default:
      jj_la1[4] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
}

  final private void values(ParserAction buffer) throws ParseException, picante.spice.kernelpool.parser.time.ParseException {Token t;
    switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
    case QUOTED_STRING:{
      label_2:
      while (true) {
        t = jj_consume_token(QUOTED_STRING);
buffer.strings.add(processQuotedString(t));
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case QUOTED_STRING:{
          ;
          break;
          }
        default:
          jj_la1[5] = jj_gen;
          break label_2;
        }
      }
buffer.isStringValued=true;
      break;
      }
    case DATE:
    case NUMBER:{
      label_3:
      while (true) {
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case DATE:{
          t = jj_consume_token(DATE);
buffer.doubles.add(processDate(t));
          break;
          }
        case NUMBER:{
          t = jj_consume_token(NUMBER);
buffer.doubles.add(processDouble(t));
          break;
          }
        default:
          jj_la1[6] = jj_gen;
          jj_consume_token(-1);
          throw new ParseException();
        }
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case DATE:
        case NUMBER:{
          ;
          break;
          }
        default:
          jj_la1[7] = jj_gen;
          break label_3;
        }
      }
buffer.isStringValued=false;
      break;
      }
    default:
      jj_la1[8] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    label_4:
    while (true) {
      switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
      case NEWLINE:{
        ;
        break;
        }
      default:
        jj_la1[9] = jj_gen;
        break label_4;
      }
      jj_consume_token(NEWLINE);
    }
}

  final private void listValues(ParserAction buffer) throws ParseException, picante.spice.kernelpool.parser.time.ParseException {Token t;
    jj_consume_token(LEFT_PAREN);
    switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
    case LIST_QUOTED_STRING:{
      label_5:
      while (true) {
        t = jj_consume_token(LIST_QUOTED_STRING);
buffer.strings.add(processQuotedString(t));
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case LIST_QUOTED_STRING:{
          ;
          break;
          }
        default:
          jj_la1[10] = jj_gen;
          break label_5;
        }
      }
buffer.isStringValued=true;
      break;
      }
    case LIST_DATE:
    case LIST_NUMBER:{
      label_6:
      while (true) {
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case LIST_DATE:{
          t = jj_consume_token(LIST_DATE);
buffer.doubles.add(processDate(t));
          break;
          }
        case LIST_NUMBER:{
          t = jj_consume_token(LIST_NUMBER);
buffer.doubles.add(processDouble(t));
          break;
          }
        default:
          jj_la1[11] = jj_gen;
          jj_consume_token(-1);
          throw new ParseException();
        }
        switch ((jj_ntk==-1)?jj_ntk_f():jj_ntk) {
        case LIST_DATE:
        case LIST_NUMBER:{
          ;
          break;
          }
        default:
          jj_la1[12] = jj_gen;
          break label_6;
        }
      }
buffer.isStringValued=false;
      break;
      }
    default:
      jj_la1[13] = jj_gen;
      jj_consume_token(-1);
      throw new ParseException();
    }
    jj_consume_token(RIGHT_PAREN);
}

  /** Generated Token Manager. */
  public ParserTokenManager token_source;
  SimpleCharStream jj_input_stream;
  /** Current token. */
  public Token token;
  /** Next token. */
  public Token jj_nt;
  private int jj_ntk;
  private int jj_gen;
  final private int[] jj_la1 = new int[14];
  static private int[] jj_la1_0;
  static {
	   jj_la1_init_0();
	}
	private static void jj_la1_init_0() {
	   jj_la1_0 = new int[] {0x300,0x24c000,0x80,0x1800,0x300,0x200000,0xc000,0xc000,0x20c000,0x80000,0x10000000,0x3000000,0x3000000,0x13000000,};
	}

  /** Constructor with InputStream. */
  public Parser(java.io.InputStream stream) {
	  this(stream, null);
  }
  /** Constructor with InputStream and supplied encoding */
  public Parser(java.io.InputStream stream, String encoding) {
	 try { jj_input_stream = new SimpleCharStream(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
	 token_source = new ParserTokenManager(jj_input_stream);
	 token = new Token();
	 jj_ntk = -1;
	 jj_gen = 0;
	 for (int i = 0; i < 14; i++) jj_la1[i] = -1;
  }

  /** Reinitialise. */
  public void ReInit(java.io.InputStream stream) {
	  ReInit(stream, null);
  }
  /** Reinitialise. */
  public void ReInit(java.io.InputStream stream, String encoding) {
	 try { jj_input_stream.ReInit(stream, encoding, 1, 1); } catch(java.io.UnsupportedEncodingException e) { throw new RuntimeException(e); }
	 token_source.ReInit(jj_input_stream);
	 token = new Token();
	 jj_ntk = -1;
	 jj_gen = 0;
	 for (int i = 0; i < 14; i++) jj_la1[i] = -1;
  }

  /** Constructor. */
  public Parser(java.io.Reader stream) {
	 jj_input_stream = new SimpleCharStream(stream, 1, 1);
	 token_source = new ParserTokenManager(jj_input_stream);
	 token = new Token();
	 jj_ntk = -1;
	 jj_gen = 0;
	 for (int i = 0; i < 14; i++) jj_la1[i] = -1;
  }

  /** Reinitialise. */
  public void ReInit(java.io.Reader stream) {
	if (jj_input_stream == null) {
	   jj_input_stream = new SimpleCharStream(stream, 1, 1);
	} else {
	   jj_input_stream.ReInit(stream, 1, 1);
	}
	if (token_source == null) {
 token_source = new ParserTokenManager(jj_input_stream);
	}

	 token_source.ReInit(jj_input_stream);
	 token = new Token();
	 jj_ntk = -1;
	 jj_gen = 0;
	 for (int i = 0; i < 14; i++) jj_la1[i] = -1;
  }

  /** Constructor with generated Token Manager. */
  public Parser(ParserTokenManager tm) {
	 token_source = tm;
	 token = new Token();
	 jj_ntk = -1;
	 jj_gen = 0;
	 for (int i = 0; i < 14; i++) jj_la1[i] = -1;
  }

  /** Reinitialise. */
  public void ReInit(ParserTokenManager tm) {
	 token_source = tm;
	 token = new Token();
	 jj_ntk = -1;
	 jj_gen = 0;
	 for (int i = 0; i < 14; i++) jj_la1[i] = -1;
  }

  private Token jj_consume_token(int kind) throws ParseException {
	 Token oldToken;
	 if ((oldToken = token).next != null) token = token.next;
	 else token = token.next = token_source.getNextToken();
	 jj_ntk = -1;
	 if (token.kind == kind) {
	   jj_gen++;
	   return token;
	 }
	 token = oldToken;
	 jj_kind = kind;
	 throw generateParseException();
  }


/** Get the next Token. */
  final public Token getNextToken() {
	 if (token.next != null) token = token.next;
	 else token = token.next = token_source.getNextToken();
	 jj_ntk = -1;
	 jj_gen++;
	 return token;
  }

/** Get the specific Token. */
  final public Token getToken(int index) {
	 Token t = token;
	 for (int i = 0; i < index; i++) {
	   if (t.next != null) t = t.next;
	   else t = t.next = token_source.getNextToken();
	 }
	 return t;
  }

  private int jj_ntk_f() {
	 if ((jj_nt=token.next) == null)
	   return (jj_ntk = (token.next=token_source.getNextToken()).kind);
	 else
	   return (jj_ntk = jj_nt.kind);
  }

  private java.util.List<int[]> jj_expentries = new java.util.ArrayList<int[]>();
  private int[] jj_expentry;
  private int jj_kind = -1;

  /** Generate ParseException. */
  public ParseException generateParseException() {
	 jj_expentries.clear();
	 boolean[] la1tokens = new boolean[29];
	 if (jj_kind >= 0) {
	   la1tokens[jj_kind] = true;
	   jj_kind = -1;
	 }
	 for (int i = 0; i < 14; i++) {
	   if (jj_la1[i] == jj_gen) {
		 for (int j = 0; j < 32; j++) {
		   if ((jj_la1_0[i] & (1<<j)) != 0) {
			 la1tokens[j] = true;
		   }
		 }
	   }
	 }
	 for (int i = 0; i < 29; i++) {
	   if (la1tokens[i]) {
		 jj_expentry = new int[1];
		 jj_expentry[0] = i;
		 jj_expentries.add(jj_expentry);
	   }
	 }
	 int[][] exptokseq = new int[jj_expentries.size()][];
	 for (int i = 0; i < jj_expentries.size(); i++) {
	   exptokseq[i] = jj_expentries.get(i);
	 }
	 return new ParseException(token, exptokseq, tokenImage);
  }

  private boolean trace_enabled;

/** Trace enabled. */
  final public boolean trace_enabled() {
	 return trace_enabled;
  }

  /** Enable tracing. */
  final public void enable_tracing() {
  }

  /** Disable tracing. */
  final public void disable_tracing() {
  }

}
