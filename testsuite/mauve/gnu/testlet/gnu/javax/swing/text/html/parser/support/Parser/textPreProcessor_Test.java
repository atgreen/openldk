// Tags: JDK1.2 GNU
// Uses: TestCase

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

package gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser;

import gnu.javax.swing.text.html.parser.support.textPreProcessor;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class textPreProcessor_Test
  extends TestCase
  implements Testlet
{
  textPreProcessor p = new textPreProcessor();

  public void test(TestHarness harness)
  {
    h = harness;
    testPreFormattedPreProcessing();
    testStandardPreProcessing();
  }

  public void testPreFormattedPreProcessing()
  {
    verifyF("rnrn...r.n.Q.Q.r.n.rn.Q...r.r.rn", "n...n.n.Q.Q.n.n.n.Q...n.n.");
    verifyF("...r.n.Q.Q.r.n.rn.Q...r.r.n", "...n.n.Q.Q.n.n.n.Q...n.n.");
    verifyF("r...r.n.Q.Q.r.n.rn.Q...r.r.n", "...n.n.Q.Q.n.n.n.Q...n.n.");
    verifyF("Q", "Q");
    verifyF(".", ".");
    verifyF("abc..\t..xyz", "abc..\t..xyz");
    verifyF("abcxyz", "abcxyz");
  }

  public void testStandardPreProcessing()
  {
    verifyS("...r.n.Q.Q.r.n.rn.Q...r.r.n", "Q.Q.Q");
    verifyS("r...r.n.Q.Q.r.n.rn.Q...r.r.n", "Q.Q.Q");
    verifyS("Q", "Q");
    verifyS(" ", null);
    verifyS(" \r\n", null);
    verifyS("abc..\t..xyz", "abc.xyz");
    verifyS("abcxyz", "abcxyz");
  }

  StringBuffer fromText(String x)
  {
    StringBuffer b = new StringBuffer();
    char c;
    for (int i = 0; i < x.length(); i++)
      {
        c = x.charAt(i);

        if (c == 'n')
          b.append('\n');
        else if (c == 'r')
          b.append('\r');
        else if (c == '.')
          b.append(' ');
        else
          b.append(c);
      }
    return b;
  }

  StringBuffer toText(String x)
  {
    StringBuffer b = new StringBuffer();
    char c;
    for (int i = 0; i < x.length(); i++)
      {
        c = x.charAt(i);

        if (c == '\n')
          b.append('n');
        else if (c == '\r')
          b.append('r');
        else if (c == ' ')
          b.append('.');
        else
          b.append(c);
      }
    return b;
  }

  void verifyF(String text, String result)
  {
    char[] pp = p.preprocessPreformatted(fromText(text));

    if (result == null && pp == null)
      return;

    String processed = new String(pp);

    processed = toText(processed).toString();

    if (!processed.equals(result))
      {
        System.err.println(result);
        System.out.println(processed);
      }
    assertEquals(text, result, processed);
  }

  void verifyS(String text, String result)
  {
    char[] pp = p.preprocess(fromText(text));

    if (result == null && pp == null)
      return;

    String processed = new String(pp);

    processed = toText(processed).toString();

    if (!processed.equals(result))
      {
        System.err.println(result);
        System.out.println(processed);
      }
    assertEquals(text, result, processed);
  }
}
