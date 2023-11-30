// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <roman@kennke.org>

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

package gnu.testlet.javax.swing.tree.DefaultTreeSelectionModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreePath;

/**
 * Checks if the constructor of DefaultTreeSelectionModel initializes
 * the object correctly.
 *
 * @author Roman Kennke
 */
public class constructor implements Testlet
{
  public void test(TestHarness h)
  {
    DefaultTreeSelectionModel m = new DefaultTreeSelectionModel();
    h.check(m.getLeadSelectionPath(), null, "getLeadSelectionPath()");
    h.check(m.getLeadSelectionRow(), -1, "getLeadSelectionRow()");
    h.check(m.getMaxSelectionRow(), -1, "getMaxSelectionRow()");
    h.check(m.getMinSelectionRow(), -1, "getMinSelectionRow()");
    h.check(m.getRowMapper(), null, "getRowMapper()");
    h.check(m.getSelectionCount(), 0, "getSelectionCount()");
    h.check(m.getSelectionMode(),
            DefaultTreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION,
            "getSelectionMode()");
    h.check(m.getSelectionPath(), null, "getSelectionPath()");
    h.check(m.getSelectionPaths(), null, "getSelectionPaths()");
    h.check(m.getSelectionRows(), null, "getSelectionRows()");
    h.check(m.isSelectionEmpty(), true, "isSelectionEmpty()");
    
  }

}
