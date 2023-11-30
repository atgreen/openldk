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


package gnu.testlet.gnu.javax.swing.text.html.parser.support.low.Constants;

import gnu.javax.swing.text.html.parser.support.low.Buffer;
import gnu.javax.swing.text.html.parser.support.low.Constants;
import gnu.javax.swing.text.html.parser.support.low.Token;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.TestCase;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Constants_Test
  extends TestCase
  implements Testlet
{
  Constants c = new Constants();

  public void test(TestHarness harness)
  {
    h = harness;
    testCases();
  }

  public void testCases()
  {
    verify("x stYle ", c.STYLE, "stYle");
    verify("x !style!", c.STYLE, "style");
    verify("x !Script!", c.SCRIPT, "Script");
    verify(" \r\t\n    z", c.WS, " \r\t\n    ");
    verify("123 ", c.NUMTOKEN, "123");
    verify("AaB123#", c.NUMTOKEN, "AaB123");
    verify("x-- ", c.DOUBLE_DASH, "--");
    verify("x--- ", c.DOUBLE_DASH, "--");

    verify("z&entitu ", c.ENTITY, "&entitu");

    verifyNull("x stYle");
    verifyNull("x !style");
    verifyNull("x !Script");
    verifyNull(" \r\t\n    ");
    verifyNull("123");
    verifyNull("AaB123");
    verifyNull("x--");
  }

  public void verify(String sequence, int kind, String image)
  {
    Token t = c.endMatches(new Buffer(sequence));
    assertEquals(kind, t.kind);
    assertEquals(image, t.getImage());
  }

  public void verifyNull(String sequence)
  {
    Token t = c.endMatches(new Buffer(sequence));
    assertNull("The end should not match any token", t);
  }
}
