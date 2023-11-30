// Tags: JDK1.4

// Copyright (C) 2004 Andrew John Hughes <gnu_andrew@member.fsf.org>

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

package gnu.testlet.javax.swing.SpinnerListModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.ArrayList;
import javax.swing.SpinnerListModel;

public class Constructors implements Testlet
{

  public void test(TestHarness harness)
  {
    SpinnerListModel model;
    boolean threwException = false;

    /* Invalid values */
    try
      {
        model = new SpinnerListModel((Object[]) null);
      }
    catch (IllegalArgumentException exception)
      {
	threwException = true;
      }
    harness.check(threwException, "Null array to constructor exception check");
    threwException = false;
    try
      {
        model = new SpinnerListModel(new ArrayList());
      }
    catch (IllegalArgumentException exception)
      {
	threwException = true;
      }
    harness.check(threwException, "Empty list to constructor exception check");
    threwException = false;
    try
      {
        model = new SpinnerListModel(new Object[]{});
        harness.fail("Empty array supplied to constructor failed to throw an exception.");
      }
    catch (IllegalArgumentException exception)
      {
	threwException = true;
      }
    harness.check(threwException, "Empty array to constructor exception check");
    /* Test the default model */
    model = new SpinnerListModel();
    harness.check(model.getList() != null, "Default list construction check.");
    harness.check(model.getValue(), "empty", "Default list current value check.");
    harness.check(model.getNextValue(), null, "Default list next value check.");
    harness.check(model.getPreviousValue(), null, "Default list previous value check.");

  }
}
