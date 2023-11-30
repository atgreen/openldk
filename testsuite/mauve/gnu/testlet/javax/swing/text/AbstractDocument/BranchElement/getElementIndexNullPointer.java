// Tags: JDK1.2

// Copyright (C) 2005 Red Hat

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
import javax.swing.text.PlainDocument;

/**
 * Checks if AbstractDocument.Branch Element throws a NPE
 * when it has no children and getElementIndex is called.
 */
public class getElementIndexNullPointer implements Testlet
{
  public void test(TestHarness h)
  {
    PlainDocument doc = new PlainDocument();
    AbstractDocument.BranchElement b = 
      doc.new BranchElement(null, null);
    try
      {
        b.getElementIndex(0);
        h.fail("AbstractDocument.BranchElement.getElementIndex should throw NPE when it has no children");
      }
    catch (NullPointerException ex)
      {
        h.check(true);
      }
  }

}
