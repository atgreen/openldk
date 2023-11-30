/* constructors.java -- Some checks for the constructors in the JSpinner class 
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
 
package gnu.testlet.javax.swing.JSpinner;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.EventListener;

import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the constructors in the {@link JSpinner} class.
 */
public class constructors implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    constructor1(harness);
    constructor2(harness);
  }
  
  public void constructor1(TestHarness harness) 
  {
    harness.checkPoint("()");
    
    // use a known look and feel
    MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
    try
      {
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e)
      {
        harness.fail(e.toString());  
      }
    
    SpinnerNumberModel m = new SpinnerNumberModel();
    JSpinner spinner = new JSpinner(m);
    harness.check(spinner.getValue(), new Integer(0));
    harness.check(spinner.getEditor() instanceof JSpinner.NumberEditor);
    
    // the model has a listener, but it isn't the spinner, editor or UI
    // some private class is listening and passing on events
    EventListener[] mListeners = m.getListeners(ChangeListener.class);
    harness.check(mListeners.length, 1);
    harness.check(!Arrays.asList(mListeners).contains(spinner));
    harness.check(!Arrays.asList(mListeners).contains(spinner.getUI()));
    harness.check(!Arrays.asList(mListeners).contains(spinner.getEditor()));
  }

  public void constructor2(TestHarness harness) 
  {
    harness.checkPoint("(SpinnerModel)");
    SpinnerNumberModel m = new SpinnerNumberModel(5, 0, 10, 1);
    JSpinner spinner = new JSpinner(m);
    harness.check(spinner.getValue(), new Integer(5));
    harness.check(spinner.getEditor() instanceof JSpinner.NumberEditor);
    
    // try null model
    boolean pass = false;
    try
    {
      /*JSpinner s =*/ new JSpinner(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
  }

}

