// Tags: JDK1.2 GNU
// Uses: TestCase Parser_Test

// supplementaryNotifications.java --
//   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class supplementaryNotifications
  extends TestCase
  implements Testlet
{
  String eoln = null;
  int flushed = 0;

  public void test(TestHarness harness)
  {
    try
      {
        h = harness;
        testHTMLParsing();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        harness.fail("Exception: " + ex);
      }
  }

  public void testHTMLParsing()
                       throws Exception
  {
    Parser_Test v =
      new Parser_Test()
      {
        public void handleEndOfLineString(String end_of_line)
        {
          eoln = end_of_line;
        }

        public void flush()
        {
          flushed++;
        }
      };

    v.hideImplied = true;

    v.verify("a \n b", "<html><head></head><body>'a b'</body></html>");

    assertEquals(eoln, "\n");

    v.verify("a \r b", "<html><head></head><body>'a b'</body></html>");

    assertEquals(eoln, "\r");

    v.verify("a \r\n b", "<html><head></head><body>'a b'</body></html>");

    assertEquals(eoln, "\r\n");

    assertEquals(flushed, 3);
  }
}
