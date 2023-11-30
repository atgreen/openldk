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
import java.util.List;
import javax.swing.SpinnerListModel;

public class SetList implements Testlet
{

public void test(TestHarness harness)
{
    SpinnerListModel model;
    List list;
    boolean threwException;

    /* Create default model */
    model = new SpinnerListModel();
    /* Invalid values */
    threwException = false;
    try
      {
        model.setList((ArrayList) null);
        harness.fail("Null list supplied to setList failed to throw an exception.");
      }
    catch (IllegalArgumentException exception)
      {
        threwException = true;
      }
    harness.check(threwException, "setList null list exception check.");
    threwException = false;
    try
      {
        model.setList(new ArrayList());
      }
    catch (IllegalArgumentException exception)
      {
        threwException = true;
      }
    harness.check(threwException, "setList empty list exception check.");
    /* Test a successful change */
    list = new ArrayList();
    list.add("GNU");
    model.setList(list);
    harness.check(model.getList(), list, "Model allowed successful change of list.");
  }

}
