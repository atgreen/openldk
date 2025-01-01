// Tags: JDK1.2

// Copyright (C) 2003, 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.javax.swing.undo.UndoableEditSupport;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.undo.UndoableEditSupport;

/**
 * Checks whether the UndoableEditSupport.getUpdateLevel
 * method works correctly.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getUpdateLevel
  implements Testlet
{
  public void test(TestHarness harness)
  {
    UndoableEditSupport ues;

    // Check #1.
    ues = new UndoableEditSupport();
    harness.check(ues.getUpdateLevel(), 0);

    // Check #2.
    ues.beginUpdate();
    harness.check(ues.getUpdateLevel(), 1);

    // Check #3.
    ues.beginUpdate();
    harness.check(ues.getUpdateLevel(), 2);

    // Check #4.
    ues.endUpdate();
    harness.check(ues.getUpdateLevel(), 1);

    // Check #5.
    ues.beginUpdate();
    harness.check(ues.getUpdateLevel(), 2);

    // Check #6.
    ues.endUpdate();
    harness.check(ues.getUpdateLevel(), 1);

    // Check #7.
    ues.endUpdate();
    harness.check(ues.getUpdateLevel(), 0);
  }
}
