// Tags: JDK1.2

// Uses: TestList

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
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JList.AccessibleJList;

import javax.accessibility.AccessibleState;
import javax.accessibility.AccessibleStateSet;
import javax.swing.ListSelectionModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the functionality of the method getAccessibleStateSet of
 * javax.swing.JList.AccessibleJList.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getAccessibleStateSet implements Testlet
{

  public void test(TestHarness harness)
  {
    TestList l = new TestList();
    TestList.AccessibleTestList al =
      (TestList.AccessibleTestList) l.getAccessibleContext();

    l.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    AccessibleStateSet ss = al.getAccessibleStateSet();
    harness.check(ss.contains(AccessibleState.MULTISELECTABLE));

    l.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    ss = al.getAccessibleStateSet();
    harness.check(ss.contains(AccessibleState.MULTISELECTABLE));

    l.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    ss = al.getAccessibleStateSet();
    harness.check(! ss.contains(AccessibleState.MULTISELECTABLE));
  }

}
