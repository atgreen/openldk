/* setSelected2.java -- some checks for the setSelected() method in the
       JInternalFrame class.
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.JInternalFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;

public class setSelected2 
  implements Testlet, PropertyChangeListener, VetoableChangeListener,
      InternalFrameListener
{
  PropertyChangeEvent lastEvent;
  
  PropertyChangeEvent lastVetoableEvent;
  
  List events = new ArrayList();
  
  public void propertyChange(PropertyChangeEvent event)
  {
    lastEvent = event;
  }
  
  public void vetoableChange(PropertyChangeEvent event)
  {
    lastVetoableEvent = event;
  }
  
  public void test(TestHarness harness)
  {
    JInternalFrame f = new JInternalFrame("F1");
    harness.check(!f.isSelected());

    // setSelected() only has an effect if the internal frame
    // is showing.
    JFrame fr = new JFrame();
    fr.getContentPane().add(f);
    f.setVisible(true);
    fr.pack();
    fr.setVisible(true);

    f.addVetoableChangeListener(this);
    f.addPropertyChangeListener(this);
    f.addInternalFrameListener(this);
    try
      {
        f.setSelected(true);
      }
    catch (PropertyVetoException e)
      {
        e.printStackTrace();
      }
    harness.check(f.isSelected());
    
    // check the vetoable event
    harness.check(lastVetoableEvent.getPropertyName(), 
            JInternalFrame.IS_SELECTED_PROPERTY);
    harness.check(lastEvent.getSource(), f);
    harness.check(lastEvent.getOldValue(), Boolean.FALSE);
    harness.check(lastEvent.getNewValue(), Boolean.TRUE);
    
    harness.check(lastEvent.getPropertyName(), 
            JInternalFrame.IS_SELECTED_PROPERTY);
    harness.check(lastEvent.getSource(), f);
    harness.check(lastEvent.getOldValue(), Boolean.FALSE);
    harness.check(lastEvent.getNewValue(), Boolean.TRUE);
    
    harness.check(events.size(), 1);
    InternalFrameEvent event = (InternalFrameEvent) events.get(0);
    harness.check(event.getSource(), f);
    harness.check(event.getID(), InternalFrameEvent.INTERNAL_FRAME_ACTIVATED);
    
    // set selected to true
    events.clear();
    try
      {
        f.setSelected(true);
      }
    catch (PropertyVetoException e)
      {
        // ignore  
      }
    harness.check(f.isSelected());
    harness.check(events.size(), 0);

    // clean up frame from desktop
    f.dispose();
    fr.dispose();
  }

  public void internalFrameActivated(InternalFrameEvent event) {
    events.add(event);  
  }

  public void internalFrameClosed(InternalFrameEvent event) {
    events.add(event);  
  }

  public void internalFrameClosing(InternalFrameEvent event) {
    events.add(event);  
  }

  public void internalFrameDeactivated(InternalFrameEvent event) {
    events.add(event);  
  }

  public void internalFrameDeiconified(InternalFrameEvent event) {
    events.add(event);  
  }

  public void internalFrameIconified(InternalFrameEvent event) {
    events.add(event);  
  }

  public void internalFrameOpened(InternalFrameEvent event) {
    events.add(event);  
  }
}
