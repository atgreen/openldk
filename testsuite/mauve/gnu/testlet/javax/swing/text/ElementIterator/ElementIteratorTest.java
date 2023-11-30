// Tags: JDK1.2

// Copyright (C) 2004 Michael Koch <konqueror@gmx.de>
// Copyright (C) 2005 Red Hat, Inc.

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.text.ElementIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.*;

public class ElementIteratorTest
  implements Testlet
{
  public class DocumentImpl extends AbstractDocument
  {
    private Element root;

    public DocumentImpl()
    {
      super(new GapContent());
      root = createBranchElement(null, null);
    }
    
    public Element getDefaultRootElement()
    {
      return root;
    }

    public Element getParagraphElement(int index)
    {
      return getDefaultRootElement();
    }

    // Override it from protected to public
    public Element createBranchElement(Element parent, AttributeSet attributes)
    {
      return super.createBranchElement(parent, attributes);
    }

    // Override it from protected to public
    public Element createLeafElement(Element parent, AttributeSet attributes,
				     int p0, int p1)
    {
      return super.createLeafElement(parent, attributes, p0, p1);
    }
  }

  public void test(TestHarness harness)
  {
    DocumentImpl doc = new DocumentImpl();

    String str = (  "Two households, both alike in dignity,\n"
		  + "In fair Verona, where we lay our scene,\n"
		  + "From ancient grudge break to new mutiny,\n"
		  + "Where civil blood makes civil hands unclean.");

    try
      {
	doc.insertString(0, str, null);

	AbstractDocument.BranchElement br
	  = (AbstractDocument.BranchElement) doc.getDefaultRootElement();

	Element leaf1 = doc.createLeafElement(br, null, 0, 38);
	Element leaf2 = doc.createLeafElement(br, null, 39, 78);
	br.replace(0, 0, new Element[] { leaf1, leaf2 });

	ElementIterator iter = new ElementIterator(doc);

	harness.check(iter.first(), br, "checking first() 1");
	harness.check(iter.next(), leaf1, "checking next() 1");
	harness.check(iter.next(), leaf2, "checking next() 2");
	harness.check(iter.previous(), leaf1, "checking previous() 1");
	// Note that previous() doesn't move the iterator.
	harness.check(iter.previous(), leaf1, "checking previous() 2");
	harness.check(iter.next(), null, "checking next() 3");
	harness.check(iter.previous(), null, "checking previous() 3");

	harness.check(iter.first(), br, "checking first() 2");
	harness.check(iter.next(), leaf1,
		      "checking next() after second first");
      }
    catch (BadLocationException _)
      {
	// insertString shouldn't fail...
	harness.check(false);
      }
  }
}
