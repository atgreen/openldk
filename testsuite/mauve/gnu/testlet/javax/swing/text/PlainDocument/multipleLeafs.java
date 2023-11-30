// Tags: JDK1.2

// Copyright (C) 2005 Michael Koch <konqueror@gmx.de>

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

package gnu.testlet.javax.swing.text.PlainDocument;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.*;

public class multipleLeafs
  implements Testlet
{
  private void checkElement(TestHarness harness, Element elem,
			    AttributeSet attributes, int children,
			    Element parent, int p0, int p1,
			    boolean leaf, String name)
  {
    if (elem == null)
      {
	harness.debug("element is null");
	return;
      }
    harness.check(elem.getAttributes() != attributes, "unexpected value for Element.getAttributes()");
    if (elem.getAttributes() != null)
      harness.check(elem.getAttributes().getAttributeCount(), 0, "number of attributes");
    harness.check(elem.getElementCount(), children, "number of children");
    harness.check(elem.getParentElement() == parent, "wrong parent");
    harness.check(elem.getStartOffset(), p0, "start offset");
    harness.check(elem.getEndOffset(), p1, "end offset");
    harness.check(elem.isLeaf(), leaf, "element is leaf element");
    harness.check(elem.getName(), name, "element name");
  }

  public void test(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();

    try
      {
	doc.insertString(doc.getLength(), "Two households, both alike in dignity,\n", null);
	doc.insertString(doc.getLength(), "In fair Verona, where we lay our scene,\n", null);
	doc.insertString(doc.getLength(), "From ancient grudge break to new mutiny,\n", null);
	doc.insertString(doc.getLength(), "Where civil blood makes civil hands unclean.\n", null);
	doc.insertString(doc.getLength(), "From forth the fatal loins of these two foes\n", null);
	doc.insertString(doc.getLength(), "A pair of star-cross'd lovers take their life;\n", null);
	doc.insertString(doc.getLength(), "Whole misadventured piteous overthrows\n", null);
	doc.insertString(doc.getLength(), "Do with their death bury their parents' strife.\n", null);
	doc.insertString(doc.getLength(), "The fearful passage of their death-mark'd love,\n", null);
	doc.insertString(doc.getLength(), "And the continuance of their parents' rage,\n", null);
	doc.insertString(doc.getLength(), "Which, but their children's end, nought could remove,\n", null);
	doc.insertString(doc.getLength(), "Is now the two hours' traffic of our stage;\n", null);
	doc.insertString(doc.getLength(), "The which if you with patient ears attend,\n", null);
	doc.insertString(doc.getLength(), "What here shall miss, our toil shall strive to mend.\n", null);
      }
    catch (Exception e)
      {
	h.debug(e);
	h.fail("unexpected exception");
      }

    h.checkPoint("leafs");
    
    
    // Check for multiple leafs.
    Element root = doc.getDefaultRootElement();
    h.check(root.getElementCount(), 15);
    h.check(root.getStartOffset(), 0);
    h.check(root.getEndOffset(), 631);

    Element leaf = root.getElement(0);
    h.check(leaf.getStartOffset(), 0);
    h.check(leaf.getEndOffset(), 39);

    leaf = root.getElement(1);
    h.check(leaf.getStartOffset(), 39);
    h.check(leaf.getEndOffset(), 79);

    leaf = root.getElement(2);
    h.check(leaf.getStartOffset(), 79);
    h.check(leaf.getEndOffset(), 120);

    leaf = root.getElement(3);
    h.check(leaf.getStartOffset(), 120);
    h.check(leaf.getEndOffset(), 165);

    leaf = root.getElement(4);
    h.check(leaf.getStartOffset(), 165);
    h.check(leaf.getEndOffset(), 210);

    leaf = root.getElement(5);
    h.check(leaf.getStartOffset(), 210);
    h.check(leaf.getEndOffset(), 257);

    leaf = root.getElement(6);
    h.check(leaf.getStartOffset(), 257);
    h.check(leaf.getEndOffset(), 296);

    leaf = root.getElement(7);
    h.check(leaf.getStartOffset(), 296);
    h.check(leaf.getEndOffset(), 344);

    leaf = root.getElement(8);
    h.check(leaf.getStartOffset(), 344);
    h.check(leaf.getEndOffset(), 392);

    leaf = root.getElement(9);
    h.check(leaf.getStartOffset(), 392);
    h.check(leaf.getEndOffset(), 436);

    leaf = root.getElement(10);
    h.check(leaf.getStartOffset(), 436);
    h.check(leaf.getEndOffset(), 490);

    leaf = root.getElement(11);
    h.check(leaf.getStartOffset(), 490);
    h.check(leaf.getEndOffset(), 534);
  }
}
