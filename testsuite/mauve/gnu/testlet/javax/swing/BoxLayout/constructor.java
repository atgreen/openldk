// Tags: JDK1.2 

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.


package gnu.testlet.javax.swing.BoxLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AWTError;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

/**
 * Some checks for the constructor in the {@link BoxLayout} class.
 */
public class constructor implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    // a null container is accepted by the constructor but the resulting
    // layout is useless...
    BoxLayout layout = new BoxLayout(null, BoxLayout.X_AXIS);
    harness.check(layout != null);
    
    // try an invalid axis type
    boolean pass = false;
    try
      {
        layout = new BoxLayout(new JPanel(), 99);
      }
    catch (AWTError e)
      {
        pass = true;
      }
    harness.check(pass);
  }
  
}

