// Tags: JDK1.2

// Copyright (C) 2005 Audrius Meskauskas, AudriusA@Bluewin.ch

// // This file is part of Mauve.

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.text.html.parser.ParserDelegator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.StringReader;

import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.parser.ParserDelegator;

/**
 * Verifies if handleEndOfLineString(..) is correctly called, providing
 * the line separator, used in the document.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class eolnNorification
  extends HTMLEditorKit.ParserCallback
  implements Testlet
{
  String eoln = null;
  int flushed = 0;

  public void handleEndOfLineString(String end_of_line)
  {
    eoln = end_of_line;
  }

  public void parse(String html)
             throws Exception
  {
    ParserDelegator delegator = new ParserDelegator();
    delegator.parse(new StringReader(html), this, true);
  }

  public void test(TestHarness harness)
  {
    try
      {
      parse("a \n b");
      harness.check(eoln, "\n", "bug in 1.2, fixed in 1.3");
      parse("a \r b");
      harness.check(eoln, "\r", "bug in 1.2, fixed in 1.3");
      parse("a \r\n b");
      harness.check(eoln, "\r\n", "bug in 1.2, fixed in 1.3");
      }
    catch (Exception ex)
      {
      harness.fail("Unexpected exception "+ex);
      }
  }
}
