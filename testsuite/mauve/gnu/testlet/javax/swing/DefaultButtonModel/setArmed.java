/* setArmed.java -- some checks for the setArmed() method in 
       the DefaultButtonModel class.
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
// Uses: MyDefaultButtonModel

package gnu.testlet.javax.swing.DefaultButtonModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultButtonModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class setArmed implements Testlet, ActionListener, ChangeListener
{
  ChangeEvent lastChangeEvent;
  
  ActionEvent lastActionEvent;
  
  public void stateChanged(ChangeEvent e) {
    lastChangeEvent = e;
  }

  public void actionPerformed(ActionEvent e) {
    lastActionEvent = e;
  }

  public void test(TestHarness harness) 
  {
    MyDefaultButtonModel m = new MyDefaultButtonModel();
    m.addActionListener(this);
    m.addChangeListener(this);
    m.setArmed(true);
    harness.check(m.isArmed(), true);
    harness.check(m.getStateMask(), DefaultButtonModel.ENABLED | DefaultButtonModel.ARMED);
    harness.check(lastChangeEvent.getSource(), m);
    harness.check(lastActionEvent, null);
    
    // setting the same again causes no event
    lastChangeEvent = null;
    m.setArmed(true);
    harness.check(lastChangeEvent, null);
    
    // if we disable the model, it becomes unarmed
    m.setEnabled(false);
    harness.check(m.isArmed(), false);
    harness.check(m.getStateMask(), 0);
    m.setEnabled(true);
    harness.check(m.isArmed(), false);
    harness.check(m.getStateMask(), DefaultButtonModel.ENABLED);
    
    m.setEnabled(false);
    m.setArmed(true);
    harness.check(m.isArmed(), false);
    harness.check(m.getStateMask(), 0);
  }
}
