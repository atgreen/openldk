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
import javax.swing.SpinnerListModel;

public class ArrayModel implements Testlet
{

  public void test(TestHarness harness)
  {
    SpinnerListModel model;
    Object[] array;
    boolean threwException;

    /* Create array */
    array = new Object[]{"GNU", "Classpath"};
    /* Create model */
    model = new SpinnerListModel(array);
    /* Check retrieval */
    harness.check(model.getList() != null, "Array model creation check");
    harness.check(model.getValue(), "GNU", "Array model current value check");
    harness.check(model.getNextValue(), "Classpath", "Array model next value check");
    harness.check(model.getValue(), "GNU", "Array model no change of current value after next check");
    harness.check(model.getPreviousValue(), null, "Array model previous value check");
    /* Value change check */
    array[0] = "GNU's Not UNIX";
    harness.check(model.getValue(), "GNU's Not UNIX", "Array model backing list change check");
    /* Value setting check */
    model.setValue("Classpath");
    harness.check(model.getValue(), "Classpath", "Array model successful set check");
    threwException = false;
    try
      {
        model.setValue("Sun");
      }
    catch (IllegalArgumentException exception)
      {
	threwException = true;
      }
    harness.check(threwException, "Array model non-existant value exception check");
  }
}
