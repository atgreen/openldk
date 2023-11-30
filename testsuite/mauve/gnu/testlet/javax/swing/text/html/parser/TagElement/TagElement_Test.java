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

package gnu.testlet.javax.swing.text.html.parser.TagElement;

import javax.swing.text.html.parser.Element;
import javax.swing.text.html.parser.TagElement;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.*;
import java.io.*;

/**
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class TagElement_Test
  implements Testlet
{
  TestHarness harness;

  public TagElement_Test()
  {
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    try {
      HTML.Tag tags[] = HTML.getAllTags();

      for (int i = 0; i < tags.length; i++) {
        HTML.Tag t = tags[i];
        String tn = t.toString();
        Element e = DTD.getDTD("test").getElement("e");
        e.name = tn;

        TagElement te = new TagElement(e, true);
        harness.check(te.fictional());

        te = new TagElement(e);
        harness.check(! (te.fictional()));

        harness.check(te.getHTMLTag().toString(), t.toString());
        harness.check(t.breaksFlow(), te.breaksFlow());
        harness.check(t.isPreformatted(), te.isPreformatted());
      }
    }
    catch (IOException ex) {
      harness.fail(ex.getMessage());
    }
  }

}
