// Tags: not-a-test

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

package gnu.testlet.javax.swing.JComponent;

import javax.swing.JLabel;

/**
 * A simple component that won't install a UI delegate (used to check default
 * settings before the UI delegate is installed).
 */
public class MyJLabel extends JLabel {
    
    public MyJLabel(String label)
    {
      super(label);
    }
    
    public void updateUI() 
    {
      // ignore this - we want the raw component, not one that has been 
      // configured by a UI delegate
    }

}
