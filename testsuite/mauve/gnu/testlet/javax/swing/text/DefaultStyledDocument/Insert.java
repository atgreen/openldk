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
public class Insert implements Testlet
{
  /**
   * Override the package private method, exposing it here.
   */
  class OpenDocument extends DefaultStyledDocument
  {
    public void insert(int offset, ElementSpec[] data)
        throws BadLocationException
    {
      super.insert(offset, data);
    }
  }
  
  
  public void test(TestHarness harness)
  {
    MutableAttributeSet a1 = new SimpleAttributeSet();
    MutableAttributeSet a2 = new SimpleAttributeSet();

    a1.addAttribute(StyleConstants.NameAttribute, "MY_FIRST");
    a2.addAttribute(StyleConstants.NameAttribute, "MY_SECOND");

    ElementSpec s1 = new ElementSpec(a1, ElementSpec.ContentType,
                                     "1".toCharArray(), 0, 1);
    ElementSpec s2 = new ElementSpec(a2, ElementSpec.ContentType,
                                     "2".toCharArray(), 0, 1);

    OpenDocument d = new OpenDocument();

    try
      {
        d.insert(0, new ElementSpec[] { s1, s2 });

        harness.check(d.getLength(), 2, "Length");        
        harness.check("12", d.getText(0, d.getLength()));
        Element[] e = d.getRootElements();

        StringBuffer b = new StringBuffer();
        for (int i = 0; i < e.length; i++)
          {
            dump(b, e[i]);
          }
        
        String r = b.toString();
        
        // Both elements must be included somewhere. They positions must match.
        harness.check(r.indexOf("MY_FIRST:0-1") >=0);
        harness.check(r.indexOf("MY_SECOND:1-2") >=0);
        
        // Insert the third element in between.
        MutableAttributeSet a3 = new SimpleAttributeSet();
        a1.addAttribute(StyleConstants.NameAttribute, "MY_MIDDLE");
        ElementSpec sm = new ElementSpec(a1, ElementSpec.ContentType,
                                     "m".toCharArray(), 0, 1);
        
        d.insert(1, new ElementSpec[] { sm });
        harness.check(d.getLength(), 3, "Length");
        harness.check("1m2", d.getText(0, d.getLength()));
        

        b.setLength(0);
        for (int i = 0; i < e.length; i++)
          {
            dump(b, e[i]);
          }
        
        r = b.toString();
        
        harness.check(r.indexOf("MY_FIRST:0-1") >=0);
        harness.check(r.indexOf("MY_MIDDLE:1-2") >= 0);
        harness.check(r.indexOf("MY_SECOND:2-3") >=0); 
        
        // Remove the first element.
        d.remove(0, 1);
        harness.check("m2", d.getText(0, d.getLength()));        
        
        b.setLength(0);
        for (int i = 0; i < e.length; i++)
          {
            dump(b, e[i]);
          }
        
        r = b.toString();
        
        // This one must no longer be present
        harness.check(r.indexOf("MY_FIRST") < 0, r);        
        harness.check(r.indexOf("MY_MIDDLE:0-1") >= 0, r);
        harness.check(r.indexOf("MY_SECOND:1-2") >=0, r); 

        // Remove the second element which is now the last.
        d.remove(1, 1);
        harness.check("m", d.getText(0, d.getLength()));
        
        b.setLength(0);
        for (int i = 0; i < e.length; i++)
          {
            dump(b, e[i]);
          }
        
        r = b.toString();
        
        // This one must no longer be present
        harness.check(r.indexOf("MY_FIRST") < 0, r);        
        harness.check(r.indexOf("MY_MIDDLE:0-1") >= 0, r);
        harness.check(r.indexOf("MY_SECOND") < 0, r); 
        
        harness.check(d.getLength(), 1, "Length");

        // Remove the last remaining element.
        d.remove(0, d.getLength());
        
        b.setLength(0);
        
        for (int i = 0; i < e.length; i++)
          {
            dump(b, e[i]);
          }
        
        r = b.toString();
        
        // This one must no longer be present
        harness.check(r.indexOf("MY_FIRST") < 0, r);        
        harness.check(r.indexOf("MY_MIDDLE") < 0, r);
        harness.check(r.indexOf("MY_SECOND") < 0, r); 
      }
    catch (BadLocationException e)
      {
        throw new RuntimeException(e);
      }
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
