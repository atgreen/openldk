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
 
package gnu.testlet.javax.swing.JSlider;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * A change listener that simply retains a reference to the change event.
 * This allows tests to check that a required event is generated.
 */
public class MyPropertyChangeListener implements PropertyChangeListener {

  public PropertyChangeEvent event;
  
  public MyPropertyChangeListener() 
  {
    this.event = null;
  }
  public void propertyChange(PropertyChangeEvent event) 
  {
    this.event = event;    
  }
}
