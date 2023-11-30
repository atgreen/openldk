// Tags: JDK1.2

// Copyright (C) 2004 Michael Koch <konqueror@gmx.de>

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

package gnu.testlet.javax.swing.text.AbstractDocument;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.*;

public class AbstractDocumentTest
  implements Testlet
{
  public class DocumentImpl extends AbstractDocument
  {
    public DocumentImpl()
    {
      super(new GapContent());
    }
    
    public Element getDefaultRootElement()
    {
      return null;
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

  private void testContentHandling(TestHarness harness)
  {
    DocumentImpl doc = new DocumentImpl();

    harness.checkPoint("testContentHandling");

    try
      {
	harness.check(doc.getText(0, doc.getLength()).equals(""));
	
	doc.insertString(0, "This is a test", null);
	harness.check(doc.getText(0, doc.getLength()).equals("This is a test"));

	doc.insertString(10, "little ", null);
	harness.check(doc.getText(0, doc.getLength()).equals("This is a little test"));

	doc.insertString(21, "case", null);
	harness.check(doc.getText(0, doc.getLength()).equals("This is a little testcase"));
      }
    catch (Exception e)
      {
	harness.fail("unexpected exception");
      }
  }

  private void checkElement(TestHarness harness, Element elem,
			    AttributeSet attributes, int children,
			    Element parent)
  {
    //harness.check(elem.getAttributes() == null, "unexpected value for Element.getAttributes()");
    harness.check(elem.getElementCount() == children, "unexpected number of children: " + elem.getElementCount());
    harness.check(elem.getParentElement() == parent, "Wrong parent");
  }

  private void testCreateLeafElement(TestHarness harness)
  {
    DocumentImpl doc = new DocumentImpl();

    harness.checkPoint("testCreateLeafElement");
    
    Element elem = doc.createLeafElement(null, null, 0, 1);
    checkElement(harness, elem, null, 0, null);
  }

  private void testCreateBranchElement(TestHarness harness)
  {
    DocumentImpl doc = new DocumentImpl();

    harness.checkPoint("testCreateBranchElement 1");
    
    Element elem;
    elem = doc.createBranchElement(null, null);
    checkElement(harness, elem, null, 0, null);

    harness.checkPoint("testCreateBranchElement 2");
    
    Element parent = elem;
    elem = doc.createBranchElement(parent, null);
    //checkElement(harness, parent, null, 1, null);
    checkElement(harness, parent, null, 0, null);
    checkElement(harness, elem, null, 0, parent);

    harness.checkPoint("testCreateBranchElement 3");
    
    Element elem2;
    elem2 = doc.createBranchElement(parent, null);
    //checkElement(harness, parent, null, 2, null);
    checkElement(harness, parent, null, 0, null);
    checkElement(harness, elem2, null, 0, parent);
  }
  
  public void test(TestHarness harness)
  {
    testCreateBranchElement(harness);
    testCreateLeafElement(harness);

    testContentHandling(harness);
  }
}
