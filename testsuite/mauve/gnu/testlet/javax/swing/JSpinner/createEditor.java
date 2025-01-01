/* createEditor.java -- Checks for the createEditor() method in the JSpinner 
                        class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4
// Uses: MyJSpinner

package gnu.testlet.javax.swing.JSpinner;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.EventListener;

import javax.swing.JComponent;
import javax.swing.JSpinner;
import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerListModel;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeListener;

public class createEditor implements Testlet 
{
  public void test(TestHarness harness) 
  {
    harness.checkPoint("(SpinnerModel)");
    MyJSpinner s1 = new MyJSpinner();
    SpinnerModel m1 = new SpinnerNumberModel();
    JComponent e1 = s1.createEditor(m1);
    harness.check(e1 instanceof JSpinner.NumberEditor);
    // no listeners added to the editor yet
    EventListener[] e1l = e1.getListeners(ChangeListener.class);
    harness.check(e1l.length, 0);
    
    SpinnerModel m2 = new SpinnerDateModel();
    MyJSpinner s2 = new MyJSpinner(m2);
    JComponent e2 = s2.createEditor(m2);
    harness.check(e2 instanceof JSpinner.DateEditor);
    // no listeners added to the editor yet
    EventListener[] e2l = e2.getListeners(ChangeListener.class);
    harness.check(e2l.length, 0);
    
    SpinnerModel m3 = new SpinnerListModel();
    MyJSpinner s3 = new MyJSpinner(m3);
    JComponent e3 = s3.createEditor(m3);
    harness.check(e3 instanceof JSpinner.ListEditor);
    // no listeners added to the editor yet
    EventListener[] e3l = e3.getListeners(ChangeListener.class);
    harness.check(e3l.length, 0);
    
    // try null argument
    e3 = s3.createEditor(null);  
    harness.check(e3 instanceof JSpinner.DefaultEditor);
  }
}
