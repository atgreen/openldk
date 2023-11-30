// Tags: JDK1.2 GNU

// Copyright (C) 2005, 2006 Audrius Meskauskas <audriusa@bluewin.ch>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.


package gnu.testlet.gnu.javax.swing.text.html.parser.support.low.ReaderTokenizer;

import gnu.javax.swing.text.html.parser.support.low.Constants;
import gnu.javax.swing.text.html.parser.support.low.ReaderTokenizer;
import gnu.javax.swing.text.html.parser.support.low.Token;
import gnu.javax.swing.text.html.parser.support.low.node;
import gnu.javax.swing.text.html.parser.support.low.pattern;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.TestCase;

import java.io.StringReader;

import java.util.ArrayList;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ReaderTokenizer_Test
  extends TestCase
  implements Testlet
{
  ReaderTokenizer rt = new ReaderTokenizer();

  public void test(TestHarness harness)
  {
    h = harness;
    try
      {
        testComplexToken();
        testReadingAndAhead();
      }
    catch (Exception ex)
      {
        h.fail("Excpetion " + ex);
      }
  }

  public void testComplexToken()
                        throws Exception
  {
    String x = "< style  >x";

    pattern a =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.NUMTOKEN),
                    new node(Constants.END), new node(Constants.NUMTOKEN)
                  }
                 );

    pattern b =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.STYLE),
                    new node(Constants.END), new node(Constants.NUMTOKEN)
                  }
                 );

    pattern c =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.WS, true),
                    new node(Constants.STYLE), new node(Constants.WS, true),
                    new node(Constants.END), new node(Constants.NUMTOKEN)
                  }
                 );

    pattern d =
      new pattern(new node[]
                  {
                    new node(Constants.BEGIN), new node(Constants.WS, true),
                    new node(Constants.STYLE), new node(Constants.WS, true),
                    new node(Constants.END), new node(Constants.BEGIN)
                  }
                 );

    ReaderTokenizer rt = new ReaderTokenizer();
    rt.reset(new StringReader(x));

    assertFalse(a.matches(rt));
    assertFalse(b.matches(rt));
    assertTrue(c.matches(rt));
    assertFalse(d.matches(rt));
  }

  public void testReadingAndAhead()
                           throws Exception
  {
    ArrayList tokens = new ArrayList();
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < 10; i++)
      {
        String r = rs();
        b.append(" ");
        b.append(r + i);
        tokens.add(" ");
        tokens.add(r + i);
      }
    rt.reset(new StringReader(b.toString()));

    for (int i = 0; i < 10; i++)
      {
        for (int ah = 0; ah < 10; ah++)
          {
            Token ahead = rt.getTokenAhead(ah);
            if (i + ah >= tokens.size())
              {
                assertEquals(ahead.kind, rt.EOF);
              }
            else
              {
                if ((i + ah) % 2 == 0)
                  assertEquals(ahead.kind, rt.WS);
                else
                  {
                    assertEquals(ahead.getImage(), tokens.get(i + ah));
                    assertEquals(ahead.kind, rt.NUMTOKEN);
                  }
              }
          }

        Token r = rt.getNextToken();
        assertEquals(r.getImage(), tokens.get(i));
      }
  }

  private String rs()
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < 10 * Math.random(); i++)
      {
        b.append("l");
      }
    return b.toString();
  }
}
