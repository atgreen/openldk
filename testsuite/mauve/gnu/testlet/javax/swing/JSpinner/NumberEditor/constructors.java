/* constructors.java -- Checks for the constructors in the NumberEditor class.
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

package gnu.testlet.javax.swing.JSpinner.NumberEditor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import javax.swing.JFormattedTextField;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.text.NumberFormatter;

public class constructors implements Testlet 
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(JSpinner)");
    JSpinner s = new JSpinner();
    JSpinner.NumberEditor e = new JSpinner.NumberEditor(s);
    harness.check(e.getLayout(), e);
    harness.check(e.getFormat(), NumberFormat.getInstance());
    JFormattedTextField ftf = e.getTextField();
    NumberFormatter nf = (NumberFormatter) ftf.getFormatter();
    harness.check(nf.getMinimum(), null);
    harness.check(nf.getMaximum(), null);
    
    SpinnerNumberModel m = new SpinnerNumberModel(50.0, 0.0, 100.0, 5.0);
    s = new JSpinner(m);
    e = new JSpinner.NumberEditor(s);
    harness.check(e.getFormat(), NumberFormat.getInstance());
    ftf = e.getTextField();
    nf = (NumberFormatter) ftf.getFormatter();
    harness.check(nf.getMinimum(), new Double(0.0));
    harness.check(nf.getMaximum(), new Double(100.0));
    
    // try null argument
    boolean pass = false;
    try
      {
        e = new JSpinner.NumberEditor(null);  
      }
    catch (NullPointerException npe)
      {
        pass = true;
      }
    harness.check(pass);
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(JSpinner, String)"); 
    JSpinner s = new JSpinner();
    JSpinner.NumberEditor e = new JSpinner.NumberEditor(s, "0.00");
    harness.check(e.getLayout(), e);
    harness.check(e.getFormat(), new DecimalFormat("0.00"));
    
    // try null spinner argument
    boolean pass = false;
    try
      {
        e = new JSpinner.NumberEditor(null, "0.00");  
      }
    catch (NullPointerException npe)
      {
        pass = true;
      }
    harness.check(pass);

    // try null format argument
    pass = false;
    try
      {
        e = new JSpinner.NumberEditor(s, null);  
      }
    catch (NullPointerException npe)
      {
        pass = true;
      }
    harness.check(pass);  
  }

}
