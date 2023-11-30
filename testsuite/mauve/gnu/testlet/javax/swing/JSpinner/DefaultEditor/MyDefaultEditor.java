/* MyDefaultEditor.java -- Exposes some methods in DefaultEditor
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

// Tags: not-a-test

package gnu.testlet.javax.swing.JSpinner.DefaultEditor;

import java.beans.PropertyChangeEvent;
import java.util.List;

import javax.swing.JSpinner;
import javax.swing.event.ChangeEvent;

public class MyDefaultEditor extends JSpinner.DefaultEditor 
{
  public List stateChangeEvents = new java.util.ArrayList();
    
  public List propertyChangeEvents = new java.util.ArrayList();
  
  public MyDefaultEditor(JSpinner spinner)
  {
    super(spinner);
  }
  
  public void stateChanged(ChangeEvent event) 
  {
    super.stateChanged(event);
    if (stateChangeEvents != null)
      stateChangeEvents.add(event);
  }
  
  public void propertyChange(PropertyChangeEvent event)
  {
    super.propertyChange(event);
    if (propertyChangeEvents != null)
      propertyChangeEvents.add(event);
  }
}
