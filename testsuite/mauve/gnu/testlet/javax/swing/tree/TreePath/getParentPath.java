// Tags: JDK1.2

// Copyright (C) 2004  Michael Koch <konqueror@gmx.de>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.tree.TreePath;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.tree.*;

/**
 * Tests if getParentPath correctly returns null if the current TreePath only
 * contains one element.
 */
public class getParentPath implements Testlet
{
  public void test(TestHarness h)
  {
    TreePath test = new TreePath("Test");
    h.check(test.getParentPath(), null);
    
    TreePath p = new TreePath(new Object[] {"A", "B"});
    TreePath parent = p.getParentPath();
    h.check(parent, new TreePath("A"));
    
    p = new TreePath(new Object[] {"A", "B", "C", "D"});
    parent = p.getParentPath();
    h.check(parent, new TreePath(new Object[] {"A", "B", "C"})); 
  }
}
