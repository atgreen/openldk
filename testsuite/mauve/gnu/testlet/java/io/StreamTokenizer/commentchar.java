// Test of commentChar() method of StreamTokenizer.

/*************************************************************************
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.1

package gnu.testlet.java.io.StreamTokenizer;

import java.io.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class commentchar implements Testlet
{
  public static void tokenize (TestHarness harness,
			       StreamTokenizer st,
			       String input,
			       int[] expected)
  {
    harness.checkPoint (input);
    try
      {
	int tt;
	int i = 0;
	while ((tt = st.nextToken ()) != StreamTokenizer.TT_EOF)
	  {
	    if (i >= expected.length)
	      harness.fail ("not enough tokens");
	    else
	      harness.check (tt, expected[i]);
	    ++i;
	  }
	harness.check (i, expected.length);
      }
    catch (Throwable _xx)
      {
	harness.debug (_xx);
	harness.fail ("Exception caught");
      }
  }

  public static StreamTokenizer make_tokenizer (String input)
  {
    StringReader sr = new StringReader (input);
    StreamTokenizer st = new StreamTokenizer (sr);
    return st;
  }

  public static void tokenize (TestHarness harness,
			       String input,
			       int[] expected)
  {
    StreamTokenizer st = make_tokenizer (input);
    st.commentChar('#');
    tokenize (harness, st, input, expected);
  }

  public void test (TestHarness harness)
  {
    int[] x1 = new int[2];
    x1[0] = StreamTokenizer.TT_WORD;
    x1[1] = StreamTokenizer.TT_WORD;
    tokenize (harness, "alpha # bleh\nbeta", x1);

    int[] x2 = new int[1];
    x2[0] = StreamTokenizer.TT_WORD;
    tokenize (harness, "alpha / bleh", x2);
    tokenize (harness, "alpha # bleh", x2);

    // Classpath regression test.
    x2[0] = StreamTokenizer.TT_EOL;
    String input = "  %foo,bar baz\n";
    StreamTokenizer st = make_tokenizer(input);
    st.resetSyntax();
    st.whitespaceChars(0, ' ');
    st.wordChars(' '+1, '\u00FF');
    st.whitespaceChars(',', ',');
    st.commentChar('%');
    st.eolIsSignificant(true);
    tokenize (harness, st, input, x2);
  }
}
