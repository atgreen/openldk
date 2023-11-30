/* setGroup.java -- some checks for the setGroup() method in 
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

package gnu.testlet.javax.swing.DefaultButtonModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.DefaultButtonModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class setGroup implements Testlet, ActionListener, ChangeListener
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
    DefaultButtonModel m = new DefaultButtonModel();
    m.addActionListener(this);
    m.addChangeListener(this);
    ButtonGroup bg = new ButtonGroup();
    m.setGroup(bg);
    harness.check(m.getGroup(), bg);
    harness.check(lastChangeEvent, null);
    harness.check(lastActionEvent, null);
    harness.check(bg.getButtonCount(), 0);
    
    // setting the same again causes no event
    lastChangeEvent = null;
    m.setGroup(bg);
    harness.check(lastChangeEvent, null);
    
    // try a null argument
    m.setGroup(null);
    harness.check(m.getGroup(), null);
    harness.check(lastChangeEvent, null);
  }
}
