// Tags: JDK1.2
// Uses: ../../../../../../gnu/javax/swing/text/html/parser/support/Parser/TestCase

// Copyright (C) 2005 Audrius Meskauskas <audriusa@bluewin.ch>

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


package gnu.testlet.javax.swing.text.html.parser.TagElement;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.TestCase;

import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.TagElement;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class TagElementTest2
  extends TestCase
  implements Testlet
{
  public void test(TestHarness harness)
  {
    h = harness;
    try
      {
        testTagElement();
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        harness.fail("Exception: " + ex);
      }
  }

  public void testTagElement()
                      throws Exception
  {
    HTML.Tag[] tags = HTML.getAllTags();

    for (int i = 0; i < tags.length; i++)
      {
        HTML.Tag t = tags [ i ];
        String tn = t.toString();
        Element e = DTD.getDTD("test").getElement("e");
        e.name = tn;

        TagElement te = new TagElement(e, true);
        assertTrue(" must be fictional", te.fictional());

        te = new TagElement(e);
        assertFalse("must be non fictional", te.fictional());

        assertEquals(te.getHTMLTag().toString(), t.toString());
        assertEquals(t.breaksFlow(), te.breaksFlow());
        assertEquals(t.isPreformatted(), te.isPreformatted());
      }
  }

  protected void setUp()
                throws Exception
  {
    super.setUp();
  }

  protected void tearDown()
                   throws Exception
  {
    super.tearDown();
  }
}
