/* setSelected.java -- some checks for the setSelected() method in 
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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.DefaultButtonModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class setSelected 
  implements Testlet, ActionListener, ChangeListener, ItemListener
{
  ChangeEvent lastChangeEvent;
  
  ActionEvent lastActionEvent;
  
  ItemEvent lastItemEvent;
  
  public void stateChanged(ChangeEvent e) {
    lastChangeEvent = e;
  }

  public void actionPerformed(ActionEvent e) {
    lastActionEvent = e;
  }

  public void itemStateChanged(ItemEvent e) 
  {
    lastItemEvent = e;
  }
  
  public void test(TestHarness harness) 
  {
    DefaultButtonModel m = new DefaultButtonModel();
    m.addActionListener(this);
    m.addChangeListener(this);
    m.addItemListener(this);
    m.setSelected(true);
    harness.check(m.isSelected(), true);
    harness.check(lastChangeEvent.getSource(), m);
    harness.check(lastActionEvent, null);
    harness.check(lastItemEvent.getSource(), m);
    harness.check(lastItemEvent.getItem(), m);
    
    // setting the same again causes no event
    lastChangeEvent = null;
    lastItemEvent = null;
    m.setSelected(true);
    harness.check(lastChangeEvent, null);
    harness.check(lastItemEvent, null);
    
    // are the states independent?  Seems so.
    m.setPressed(true);
    harness.check(m.isSelected(), true);
    m.setEnabled(false);
    harness.check(m.isSelected(), true);
    m.setEnabled(true);
    harness.check(m.isSelected(), true);
  }


}
