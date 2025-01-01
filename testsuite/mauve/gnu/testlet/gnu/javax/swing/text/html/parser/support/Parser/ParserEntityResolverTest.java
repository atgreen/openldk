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

import gnu.javax.swing.text.html.parser.HTML_401F;
import gnu.javax.swing.text.html.parser.support.Parser;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.reflect.Method;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ParserEntityResolverTest
  extends TestCase
  implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
        h = harness;
        testResolver();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        harness.fail("Exception: " + ex);
      }
  }

  /* Testing private methods of entity resolver. */
  public void testResolver()
                    throws Exception
  {
    Parser p =
      new Parser(HTML_401F.getInstance())
      {
        public void error(String a, String b)
        {
        }
      };

    Method rn =
      p.getClass().getSuperclass().getDeclaredMethod("resolveNamedEntity",
                                                     new Class[] { String.class }
                                                    );
    rn.setAccessible(true);

    assertEquals(exe(p, rn, "&amp"), "&");
    assertEquals(exe(p, rn, "&AMP"), "&");
    assertEquals(exe(p, rn, "&amp"), "&");
    assertEquals(exe(p, rn, "&amP"), "&");

    assertEquals(exe(p, rn, "&;"), "&;");
    assertEquals(exe(p, rn, "&audrius;"), "&audrius;");

    rn =
      p.getClass().getSuperclass().getDeclaredMethod("resolveNumericEntity",
                                                     new Class[] { String.class }
                                                    );
    rn.setAccessible(true);

    assertEquals(exe(p, rn, "&#x55"), "U");
    assertEquals(exe(p, rn, "&#X55"), "U");
    assertEquals(exe(p, rn, "&#61"), "=");
    assertEquals(exe(p, rn, "&#61"), "=");

    assertEquals(exe(p, rn, "&#audrius"), "?");
  }

  private String exe(Parser p, Method m, String arg)
              throws Exception
  {
    Object[] o = new Object[ 1 ];
    o [ 0 ] = arg;
    return m.invoke(p, o).toString();
  }
}
