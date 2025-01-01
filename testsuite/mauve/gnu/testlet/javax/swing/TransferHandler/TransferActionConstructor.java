// Tags: JDK1.4

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

package gnu.testlet.javax.swing.TransferHandler;

import javax.swing.Action;
import javax.swing.TransferHandler;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks that the TransferAction constructor sets the Action.NAME property.
 */
public class TransferActionConstructor implements Testlet
{

  public void test(TestHarness harness)
  {
    Action a = TransferHandler.getCopyAction();
    harness.check (a.getValue(Action.NAME).equals("copy"));
  }

}
