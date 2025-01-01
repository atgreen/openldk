/* constructors.java -- Checks for the constructors in the DateEditor class.
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

package gnu.testlet.javax.swing.JSpinner.DateEditor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import javax.swing.JFormattedTextField;
import javax.swing.JSpinner;
import javax.swing.SpinnerDateModel;
import javax.swing.text.DateFormatter;

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
    SpinnerDateModel m = new SpinnerDateModel();
    JSpinner s = new JSpinner(m);
    JSpinner.DateEditor e = new JSpinner.DateEditor(s);
    harness.check(e.getFormat(), DateFormat.getInstance());
    harness.check(e.getLayout(), e);
    JFormattedTextField ftf = e.getTextField();
    DateFormatter nf = (DateFormatter) ftf.getFormatter();
    harness.check(nf.getMinimum(), null);
    harness.check(nf.getMaximum(), null);
    
    m = new SpinnerDateModel(new Date(50L), new Date(0L), new Date(100L), 
            Calendar.MILLISECOND);
    s = new JSpinner(m);
    e = new JSpinner.DateEditor(s);
    harness.check(e.getFormat(), DateFormat.getInstance());
    ftf = e.getTextField();
    nf = (DateFormatter) ftf.getFormatter();
    harness.check(nf.getMinimum(), new Date(0L));
    harness.check(nf.getMaximum(), new Date(100L));
    
    // try null argument
    boolean pass = false;
    try
      {
        e = new JSpinner.DateEditor(null);  
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
    SpinnerDateModel m = new SpinnerDateModel();
    JSpinner s = new JSpinner(m);
    JSpinner.DateEditor e = new JSpinner.DateEditor(s, "S");
    harness.check(e.getFormat(), new SimpleDateFormat("S"));
    harness.check(e.getLayout(), e);
    
    // try null spinner argument
    boolean pass = false;
    try
      {
        e = new JSpinner.DateEditor(null, "S");  
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
        e = new JSpinner.DateEditor(s, null);  
      }
    catch (NullPointerException npe)
      {
        pass = true;
      }
    harness.check(pass);  
  }

}
