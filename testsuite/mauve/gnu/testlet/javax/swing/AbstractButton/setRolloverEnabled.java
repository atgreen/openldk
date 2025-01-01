// Tags: JDK1.4

// Copyright (C) 2005, 2006 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.javax.swing.AbstractButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Some checks for the setRolloverEnabled() method in the 
 * {@link AbstractButton} class.
 */
public class setRolloverEnabled 
  implements Testlet, PropertyChangeListener 
{
  private PropertyChangeEvent event;
  
  public void propertyChange(PropertyChangeEvent event) 
  {
    this.event = event;
  }
  
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  { 
    // this test is theme-dependent
    try
      {
        MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e) 
      {
        // ignore 
      }
    AbstractButton b = new JButton("Test");
    harness.check(b.isRolloverEnabled(), false);
    b.addPropertyChangeListener(this);    
    b.setRolloverEnabled(true);
    harness.check(b.isRolloverEnabled(), true);
    harness.check(this.event.getPropertyName(), "rolloverEnabled");
    harness.check(this.event.getSource(), b);
    harness.check(this.event.getOldValue(), Boolean.FALSE);
    harness.check(this.event.getNewValue(), Boolean.TRUE);
  }
}

