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

package gnu.testlet.javax.swing.text.html.parser.DTD;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.*;


/**
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class DTD_test
  implements Testlet
{
  static class D
    extends DTD
  {
    public D()
    {
      super("test");
    }

    public Element createElement(String n)
    {
      return getElement(n);
    }
  }

  TestHarness harness;

  public DTD_test()
  {
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    D d = new D();
    HTML.Tag tags[] = HTML.getAllTags();

    Element prehead = d.createElement("head");

    for (int i = 0; i < tags.length; i++)
      {
      Element e = d.createElement(tags [ i ].toString());
      String name = tags [ i ].toString();
      harness.check(e != null, "Element creation");
      harness.check(e.getName().equalsIgnoreCase(name), "Element name");
      }

    // Test upper/lowercase
    Element e = d.createElement("head");

    harness.check((e != null), "Element creation");
    harness.check(e.getName().equalsIgnoreCase("head"), "Element name");
    harness.check(HTML.Tag.HEAD, HTML.getTag(e.name));
    harness.check(d.head, e, "Field assignment");

    harness.check(prehead, e);
  }

}
