// Tags: JDK1.2

// Uses: ../TestList

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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
// along with Mauve; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.javax.swing.JList.AccessibleJList.AccessibleJListChild;

import java.awt.Font;

import javax.accessibility.AccessibleComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.javax.swing.JList.AccessibleJList.TestList;

/**
 * Checks if the method getFont works correctly. This should return the
 * font of the actual list, since the background cannot be set on
 * the list children individually.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getFont implements Testlet
{

  public void test(TestHarness harness)
  {
    TestList l = new TestList(new String[]{"item"});
    TestList.AccessibleTestList al =
      (TestList.AccessibleTestList) l.getAccessibleContext();
    Font font1 = new Font("Dialog", Font.PLAIN, 16);
    Font font2 = new Font("Dialog", Font.BOLD, 12);
    l.setFont(font1);
    AccessibleComponent child = (AccessibleComponent) al.getAccessibleChild(0);
    harness.check(child.getFont(), font1);
    l.setFont(font2);
    harness.check(child.getFont(), font2);
  }

}
