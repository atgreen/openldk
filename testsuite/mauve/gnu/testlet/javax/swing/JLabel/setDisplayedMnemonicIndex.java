/* setDisplayedMnemonicIndex.java -- some checks for the 
       setDisplayedMnemonicIndex() method.
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JLabel;

public class setDisplayedMnemonicIndex 
    implements Testlet , PropertyChangeListener
{
  List events = new java.util.ArrayList();
  
  int mnemonicIndexWhenEventFired = -1;
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
    if (e.getPropertyName().equals("displayedMnemonicIndex") 
            && e.getSource() instanceof JLabel)
      {
        JLabel l = (JLabel) e.getSource();
        mnemonicIndexWhenEventFired = l.getDisplayedMnemonicIndex();
      }
  }

  public void test(TestHarness harness)
  {
    JLabel label = new JLabel("ABCDEFG");
    harness.check(label.getDisplayedMnemonicIndex(), -1);
    label.addPropertyChangeListener(this);
    label.setDisplayedMnemonicIndex(3);
    harness.check(label.getDisplayedMnemonicIndex(), 3);
    harness.check(label.getDisplayedMnemonic(), 0);
    harness.check(events.size(), 1);
    PropertyChangeEvent e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getSource(), label);
    harness.check(e.getPropertyName(), "displayedMnemonicIndex");
    harness.check(e.getOldValue(), new Integer(-1));
    harness.check(e.getNewValue(), new Integer(3));
    harness.check(mnemonicIndexWhenEventFired, 3);  // that is, value set 
        // before event is fired
    
    // setting the same value fires no event
    events.clear();
    label.setDisplayedMnemonicIndex(3);
    harness.check(events.size(), 0);
    
    // setting the index to -1 is OK
    events.clear();
    label.setDisplayedMnemonicIndex(-1);
    harness.check(label.getDisplayedMnemonicIndex(), -1);
    harness.check(events.size(), 1);
    e = (PropertyChangeEvent) events.get(0);
    harness.check(e.getSource(), label);
    harness.check(e.getOldValue(), new Integer(3));
    harness.check(e.getNewValue(), new Integer(-1));
    
    // setting the index to -2 should generate an IllegalArgumentException
    boolean pass = false;
    try
    {
      label.setDisplayedMnemonicIndex(-2);
    }
    catch (IllegalArgumentException ex)
    {
      pass = true;
    }
    harness.check(pass);

    // setting the index to the length of the label text should generate an 
    // IllegalArgumentException
    pass = false;
    try
    {
      label.setDisplayedMnemonicIndex(label.getText().length());
    }
    catch (IllegalArgumentException ex)
    {
      pass = true;
    }
    harness.check(pass);
    
    // setting the index to zero or greater when the text is null should 
    // generate an IllegalArgumentException
    label.setText(null);
    harness.check(label.getDisplayedMnemonicIndex(), -1);
    pass = false;
    try
    {
      label.setDisplayedMnemonicIndex(0);
    }
    catch (IllegalArgumentException ex)
    {
      pass = true;
    }
    harness.check(pass);
    
    label.setText("");
    harness.check(label.getDisplayedMnemonicIndex(), -1);
    pass = false;
    try
    {
      label.setDisplayedMnemonicIndex(0);
    }
    catch (IllegalArgumentException ex)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
