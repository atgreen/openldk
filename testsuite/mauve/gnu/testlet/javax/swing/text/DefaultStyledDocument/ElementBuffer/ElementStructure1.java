// Tags: JDK1.2

// Copyright (C) 2006 Red Hat.

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

package gnu.testlet.javax.swing.text.DefaultStyledDocument.ElementBuffer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;

public class ElementStructure1 extends DefaultStyledDocument implements Testlet
{
	/**
	 * Starts the test run.
	 * 
	 * @param harness
	 *            the test harness to use
	 */
  public void test(TestHarness harness)
  {
    ElementStructure1 doc = new ElementStructure1();
    Element root = doc.getDefaultRootElement();
    try
      {
        // In this test no new LeafElements or BranchElements should be 
        // created.  We're able to add everything to the original Element
        // that was created when the document was constructed.
        harness.checkPoint ("initial setup");
        harness.check (root.getElementCount() == 1);
        harness.check (root.getElement(0).getElementCount() == 1);
        harness.check 
          (root.getElement(0).getElement(0).getElementCount() == 0);        
        doc.insertString(0, "the quick brown fox", null);

        harness.checkPoint("after first insertion");
        harness.check (root.getElementCount() == 1);
        harness.check (root.getElement(0).getElementCount() == 1);
        harness.check 
          (root.getElement(0).getElement(0).getElementCount() == 0);
        harness.check (root.getEndOffset() == 20);
        doc.insertString(6, "MIDDLE", null);        

        harness.checkPoint("after second insertion");
        harness.check (root.getElementCount() == 1);
        harness.check (root.getElement(0).getElementCount() == 1);
        harness.check 
          (root.getElement(0).getElement(0).getElementCount() == 0);
        harness.check (root.getEndOffset() == 26);
        doc.insertString(0, "START", null);        

        harness.checkPoint("after third insertion");
        harness.check (root.getElementCount() == 1);
        harness.check (root.getElement(0).getElementCount() == 1);
        harness.check 
          (root.getElement(0).getElement(0).getElementCount() == 0);
        harness.check (root.getEndOffset() == 31);
        doc.insertString(30, "END", null);
        
        harness.checkPoint("after fourth insertion");
        harness.check(root.getEndOffset() == 34);

        // printElements(doc.getDefaultRootElement(), 0);
      }
    catch (BadLocationException ex)
      {
        // ex.printStackTrace();
        harness.debug(ex);
      }
    catch (AssertionError ex)
      {
        // ex.printStackTrace();
        harness.debug(ex);
      }
  }
}
