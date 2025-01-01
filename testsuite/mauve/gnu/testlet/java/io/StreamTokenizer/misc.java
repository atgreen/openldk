// Test of several methods of StreamTokenizer.

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

public class misc implements Testlet
{
  public void test (TestHarness harness)
  {
	int tt;		// Token type

	StringReader sr = new StringReader("LineOne\nSecond/Line\n?Question?3.14\nAxyz");
	StreamTokenizer st = new StreamTokenizer(sr);
	st.eolIsSignificant(true);	// Pass EOLs as tokens
 	st.ordinaryChar('/');		// Remove 'comment' specialness of /
	st.lowerCaseMode(true);
	try {
		tt = st.nextToken();
		harness.check(st.lineno(), 1, "lineno()");
		harness.check(st.sval, "lineone", "lowerCaseMode()");
		tt = st.nextToken();  // Should be newline		
		harness.check(tt, StreamTokenizer.TT_EOL, "eolIsSignificant()");
		tt = st.nextToken();	// Parse 'Second'
		tt = st.nextToken();	// Parse '/'
		st.lowerCaseMode(false);
		tt = st.nextToken();	// Parse 'Line'; wouldn't happen if / were a comment char
		harness.check(st.sval, "Line", "ordinaryChar()");
		st.pushBack();
		tt = st.nextToken();
		harness.check(st.sval, "Line", "pushBack()");
		st.quoteChar('?');
		tt = st.nextToken();	// Parse EOL
		tt = st.nextToken();	// Get string quoted by ?
		harness.check(st.ttype, '?', "ttype field");
		harness.check(st.sval, "Question", "quoteChar()");
		st.parseNumbers();		// Get ready for the next one
		tt = st.nextToken();
		harness.check(tt, StreamTokenizer.TT_NUMBER, "TT_NUMBER");
		harness.check(st.nval > 3.1399 && st.nval < 3.1401, "parseNumbers()");
		harness.debug("'3.14' came out " + st.nval);
		st.ordinaryChars('A','C');	// Make A, B, and C their own special tokens
		tt = st.nextToken();	// Parse EOL
		harness.check(st.nextToken(), 'A', "ordinaryChars()");
		st.resetSyntax();		// Every character is its own token
		harness.check(st.nextToken(), 'x', "resetSyntax()");
	}
	catch (IOException e) {
		harness.debug (e);
		harness.fail ("Unexpected Exception caught");
	}
  }
}
