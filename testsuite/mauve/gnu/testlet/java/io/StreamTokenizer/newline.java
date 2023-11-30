// Test of newline behavior of StreamTokenizer.

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

public class newline implements Testlet
{
  public static void tokenize (TestHarness harness,
			       String input,
			       int[] expected,
			       StreamTokenizer st)
  {
    String checkpointName = "URLEncoded " + java.net.URLEncoder.encode(input);
    harness.checkPoint ("Test of input: " + checkpointName);
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
    catch (Throwable _)
      {
	harness.debug (_);
	harness.fail ("Exception caught");
      }
  }

  public void test (TestHarness harness)
  {
    String input = "foo\nbar\r\njoe\rzardoz";
    StringReader sr = new StringReader (input);
    int[] expected = new int[7];
    expected[0] = StreamTokenizer.TT_WORD;
    expected[1] = StreamTokenizer.TT_EOL;
    expected[2] = StreamTokenizer.TT_WORD;
    expected[3] = StreamTokenizer.TT_EOL;
    expected[4] = StreamTokenizer.TT_WORD;
    expected[5] = StreamTokenizer.TT_EOL;
    expected[6] = StreamTokenizer.TT_WORD;

    StreamTokenizer st = new StreamTokenizer (sr);
    st.eolIsSignificant (true);
    tokenize (harness, input, expected, st);

    input = "\"foo\\\nbar\" after";
    sr = new StringReader (input);
    st = new StreamTokenizer (sr);
    expected = new int[2];
    expected[0] = (int) '"';
    expected[1] = StreamTokenizer.TT_WORD;
    tokenize (harness, input, expected, st);
  }
}
