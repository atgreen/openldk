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

package gnu.testlet.javax.swing.text.AbstractDocument.BranchElement;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.text.AbstractDocument;
import javax.swing.text.Element;
import javax.swing.text.GapContent;

public class BranchElementTest extends AbstractDocument
  implements Testlet
{
  public BranchElementTest()
  {
    super(new GapContent());
  }
  
  public Element getDefaultRootElement()
  {
    return null;
  }

  public Element getParagraphElement(int pos)
  {
    return null;
  }
  
  public void test(TestHarness h)
  {
    AbstractDocument.BranchElement root = new AbstractDocument.BranchElement(null, null);
    AbstractDocument.BranchElement leaf1, leaf2, leaf3;
    Element[] array1 = new Element[1];
    Element[] array2 = new Element[2];

    h.check(root.getElementCount(), 0, "number of children");

    leaf1 = new AbstractDocument.BranchElement(root, null);
    array1[0] = leaf1;
    root.replace(0, 0, array1);

    h.check(root.getElementCount(), 1, "number of children");

    leaf2 = new AbstractDocument.BranchElement(root, null);
    array2[0] = leaf1;
    array2[1] = leaf2;
    root.replace(0, 1, array2);

    h.check(root.getElementCount(), 2, "number of children");

    leaf3 = new AbstractDocument.BranchElement(leaf1, null);
    array1[0] = leaf3;
    leaf1.replace(0, 0, array1);

    h.check(root.getElementCount(), 2, "number of children");
  }
}
