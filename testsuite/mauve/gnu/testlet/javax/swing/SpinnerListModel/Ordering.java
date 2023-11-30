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

public class Ordering implements Testlet
{

  public void test(TestHarness harness)
  {
    SpinnerListModel model;
    List list;
    
    /* Create list */
    list = new ArrayList();
    list.add("a");
    list.add("z");
    list.add("a");
    list.add("b");
    /* Create model */
    model = new SpinnerListModel(list);
    /* Check retrieval */
    harness.check(model.getList() != null, "Array model ordering creation check");
    harness.check(model.getValue(), "a", "Array model ordering current value check");
    harness.check(model.getNextValue(), "z", "Array model ordering next value check");
    harness.check(model.getValue(), "a", "Array model ordering no change of current value after next check");
    harness.check(model.getPreviousValue(), null, "Array model ordering previous value check");
    /* Value ordering of setting check */
    model.setValue("a");
    harness.check(model.getValue(), "a", "Array model ordering successful set check");
    harness.check(model.getPreviousValue(), null, "Array model ordering post-set previous value check");
    harness.check(model.getNextValue(), "z", "Array model ordering post-set next value check");
  }
}
