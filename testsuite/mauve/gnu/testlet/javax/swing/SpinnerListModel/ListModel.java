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

public class ListModel implements Testlet
{

  public void test(TestHarness harness)
  {
    SpinnerListModel model;
    List list;
    boolean threwException = false;
    
    /* Create list */
    list = new ArrayList();
    list.add("GNU");
    list.add("Classpath");
    /* Create model */
    model = new SpinnerListModel(list);
    /* Check retrieval */
    harness.check(model.getList() != null, "List model creation check");
    harness.check(model.getValue(), "GNU", "List model current value check");
    harness.check(model.getNextValue(), "Classpath", "List model next value check");
    harness.check(model.getValue(), "GNU", "List model no change of current value after next check");
    harness.check(model.getPreviousValue(), null, "List model previous value check");
    /* Value change check */
    list.set(0, "GNU's Not UNIX");
    harness.check(model.getValue(), "GNU's Not UNIX", "List model backing list change check");
    /* Value setting check */
    model.setValue("Classpath");
    harness.check(model.getValue(), "Classpath", "List model successful set check");
    try
      {
        model.setValue("Sun");
      }
    catch (IllegalArgumentException exception)
      {
	threwException = true;
      }
    harness.check(threwException, "List model non-existant value exception check.");
  }
}
