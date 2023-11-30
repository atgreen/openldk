// Test of resetting word chars to whitespace chars

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

import java.io.StreamTokenizer;
import java.io.StringReader;
import java.io.IOException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class WordWhiteChars implements Testlet
{
  public void test (TestHarness harness)
  {
    StreamTokenizer st = new StreamTokenizer(new StringReader("foo bar,baz"));
    // Everything is a word character.
    st.wordChars(0, 255);
    // Except for spaces and commas
    st.whitespaceChars(' ', ' ');
    st.whitespaceChars(',', ',');

    try
      {
	harness.check(st.nextToken(), StreamTokenizer.TT_WORD);
	harness.check(st.sval, "foo");
	harness.check(st.nextToken(), StreamTokenizer.TT_WORD);
	harness.check(st.sval, "bar");
	harness.check(st.nextToken(), StreamTokenizer.TT_WORD);
	harness.check(st.sval, "baz");
	harness.check(st.nextToken(), StreamTokenizer.TT_EOF);
      }
    catch (IOException ioe)
      {
	harness.fail(ioe.toString());
      }
  }
}
