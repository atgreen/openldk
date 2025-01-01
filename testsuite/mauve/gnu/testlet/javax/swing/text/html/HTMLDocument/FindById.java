/* FindById.java -- Test getElement(String)
   Copyright (C) 2006 Audrius Meskauskas
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: 1.4

package gnu.testlet.javax.swing.text.html.HTMLDocument;

import javax.swing.JTextPane;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class FindById implements Testlet
{

  public void test(TestHarness harness)
  {
    JTextPane html = new JTextPane();
    html.setContentType("text/html");
    
    html.setText("<p id = myOne>my text<i><b id = myTwo>myBold</b></i>xxx");

    HTMLDocument doc = (HTMLDocument) html.getDocument();

    Element el = doc.getElement("myOne");

    harness.check(true, el != null, "p with id must be found");
    harness.check(
                  el.getAttributes().getAttribute(StyleConstants.NameAttribute),
                  HTML.Tag.P, "Type must match");
    harness.check(!el.isLeaf());

    el = doc.getElement("myNone");
    harness.check(el == null, "should not be found");
    
  }

}
