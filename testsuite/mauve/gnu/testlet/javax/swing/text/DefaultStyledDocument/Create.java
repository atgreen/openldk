/* Insert.java -- Test bulk element insertion
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

package gnu.testlet.javax.swing.text.DefaultStyledDocument;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.DefaultStyledDocument.ElementSpec;

/**
 * Tests the protected method to insert the elements. This method is 
 * important when modifying content of the derived HTMLDocument.
 */
public class Create extends Insert implements Testlet
{
  /**
   * Override the package private method, exposing it here.
   */
  class OpenDocument extends DefaultStyledDocument
  {
    public void create(ElementSpec[] data)
    {
      super.create(data);
    }
  }
  
  
  public void test(TestHarness harness)
  {
    MutableAttributeSet a1 = new SimpleAttributeSet();
    MutableAttributeSet a2 = new SimpleAttributeSet();
    MutableAttributeSet a3 = new SimpleAttributeSet();

    a1.addAttribute(StyleConstants.NameAttribute, "MY_FIRST");
    a2.addAttribute(StyleConstants.NameAttribute, "MY_SECOND");
    a3.addAttribute(StyleConstants.NameAttribute, "MY_MIDDLE");

    ElementSpec s1 = new ElementSpec(a1, ElementSpec.ContentType,
                                     "1".toCharArray(), 0, 1);
    ElementSpec s2 = new ElementSpec(a2, ElementSpec.ContentType,
                                     "2".toCharArray(), 0, 1);

    ElementSpec sm = new ElementSpec(a3, ElementSpec.ContentType,
                                     "m".toCharArray(), 0, 1);

    OpenDocument d = new OpenDocument();

    d.create(new ElementSpec[] { s1, sm, s2 });

    harness.check(d.getLength(), 3, "Length");
    try
      {
        harness.check(d.getText(0, d.getLength()), "1m2");
      }
    catch (BadLocationException e1)
      {
        throw new RuntimeException(e1);
      }

    Element[] e = d.getRootElements();

    StringBuffer b = new StringBuffer();
    for (int i = 0; i < e.length; i++)
      {
        dump(b, e[i]);
      }

    String r = b.toString();

    // Sun 1.5.0_06-b05 swallows the first element.
    // harness.check(r.indexOf("MY_FIRST") >= 0, r);
    harness.check(r.indexOf("MY_MIDDLE") >= 0, r);
    harness.check(r.indexOf("MY_SECOND") >= 0, r);
  }
  
  void dump(StringBuffer b, Element x)
  {
    Object name = x.getAttributes().getAttribute(StyleConstants.NameAttribute);
    b.append("( ");
    b.append(name+":"+x.getStartOffset()+"-"+x.getEndOffset());
    b.append(" ch ");
    for (int i = 0; i < x.getElementCount(); i++)
      {
        dump(b, x.getElement(i));
      }
    b.append(") ");
  }

}
