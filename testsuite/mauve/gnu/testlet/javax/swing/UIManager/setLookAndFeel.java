// Tags: JDK1.4

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.UIManager;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.LookAndFeel;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

/**
 * Some checks for the setLookAndFeel() method in the 
 * {@link UIManager} class.
 */
public class setLookAndFeel
    implements Testlet, PropertyChangeListener {

  public PropertyChangeEvent lastEvent = null;
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)  
  {
    LookAndFeel laf1 = new MyLookAndFeel();
    try
    {
      UIManager.addPropertyChangeListener(this);
      UIManager.setLookAndFeel(laf1);
      harness.check(UIManager.getLookAndFeel(), laf1);
      harness.check(this.lastEvent.getPropertyName(), "lookAndFeel");
      harness.check(this.lastEvent.getSource(), UIManager.class);
      harness.check(this.lastEvent.getNewValue(), laf1);
    }
    catch (UnsupportedLookAndFeelException e)
    {
      harness.fail(e.toString());
    }
   
    try
    {
      UIManager.setLookAndFeel((LookAndFeel) null);
      harness.check(UIManager.getLookAndFeel(), null);
      harness.check(this.lastEvent.getPropertyName(), "lookAndFeel");
      harness.check(this.lastEvent.getSource(), UIManager.class);
      harness.check(this.lastEvent.getOldValue(), laf1);
      harness.check(this.lastEvent.getNewValue(), null);
    }
    catch (UnsupportedLookAndFeelException e)
    {
      harness.fail(e.toString());
    }
        
    UIManager.removePropertyChangeListener(this);
  }
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    this.lastEvent = e;
  }
}
