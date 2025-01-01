/* setDisplayedMnemonic.java -- some checks for the setDisplayedMnemonic()
       methods in the JLabel class.
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

import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.KeyStroke;

public class setDisplayedMnemonic implements Testlet, PropertyChangeListener
{

  List events = new java.util.ArrayList();
  
  int displayedMnemonicWhenEventFired;
  
  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
    if (e.getPropertyName().equals("displayedMnemonic") 
            && e.getSource() instanceof JLabel)
    {
      JLabel label = (JLabel) e.getSource();
      displayedMnemonicWhenEventFired = label.getDisplayedMnemonic();
    }
  }

  public void test(TestHarness harness) 
  {
    testMethod1(harness);
    testMethod2(harness);
    testKeyMap(harness);
  }
  
  public void testMethod1(TestHarness harness) 
  {
    harness.checkPoint("(char)");
    JLabel label = new JLabel("Abc Def");
    label.addPropertyChangeListener(this);
    label.setDisplayedMnemonic('d');
    harness.check(label.getDisplayedMnemonic(), 68);
    harness.check(label.getDisplayedMnemonicIndex(), 4);
    harness.check(events.size(), 2);
    harness.check(displayedMnemonicWhenEventFired, 68);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "displayedMnemonic");
    harness.check(e0.getOldValue(), new Integer(0));
    harness.check(e0.getNewValue(), new Integer(68));
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(1);
    harness.check(e1.getSource(), label);
    harness.check(e1.getPropertyName(), "displayedMnemonicIndex");
    harness.check(e1.getOldValue(), new Integer(-1));
    harness.check(e1.getNewValue(), new Integer(4));
    
    // try a character that isn't in the text
    events.clear();
    label.setDisplayedMnemonic('z');
    harness.check(label.getDisplayedMnemonic(), 90);
    harness.check(label.getDisplayedMnemonicIndex(), -1);
    harness.check(events.size(), 2);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "displayedMnemonic");
    harness.check(e0.getOldValue(), new Integer(68));
    harness.check(e0.getNewValue(), new Integer(90));
    e1 = (PropertyChangeEvent) events.get(1);
    harness.check(e1.getSource(), label);
    harness.check(e1.getPropertyName(), "displayedMnemonicIndex");
    harness.check(e1.getOldValue(), new Integer(4));
    harness.check(e1.getNewValue(), new Integer(-1));
  }

  public void testMethod2(TestHarness harness) 
  {
    harness.checkPoint("(int)");
    events.clear();
    JLabel label = new JLabel("Abc Def");
    label.addPropertyChangeListener(this);
    label.setDisplayedMnemonic(68);
    harness.check(label.getDisplayedMnemonic(), 68);
    harness.check(label.getDisplayedMnemonicIndex(), 4);
    harness.check(events.size(), 2);
    harness.check(displayedMnemonicWhenEventFired, 68);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "displayedMnemonic");
    harness.check(e0.getOldValue(), new Integer(0));
    harness.check(e0.getNewValue(), new Integer(68));
    PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(1);
    harness.check(e1.getSource(), label);
    harness.check(e1.getPropertyName(), "displayedMnemonicIndex");
    harness.check(e1.getOldValue(), new Integer(-1));
    harness.check(e1.getNewValue(), new Integer(4));
    
    // try a character that isn't in the text
    events.clear();
    label.setDisplayedMnemonic(90);
    harness.check(label.getDisplayedMnemonic(), 90);
    harness.check(label.getDisplayedMnemonicIndex(), -1);
    harness.check(events.size(), 2);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "displayedMnemonic");
    harness.check(e0.getOldValue(), new Integer(68));
    harness.check(e0.getNewValue(), new Integer(90));
    e1 = (PropertyChangeEvent) events.get(1);
    harness.check(e1.getSource(), label);
    harness.check(e1.getPropertyName(), "displayedMnemonicIndex");
    harness.check(e1.getOldValue(), new Integer(4));
    harness.check(e1.getNewValue(), new Integer(-1));
  }
  
  public void testKeyMap(TestHarness harness)
  {
    JLabel label = new JLabel("ABC");
    label.setDisplayedMnemonic('B');
    InputMap m = label.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m.get(KeyStroke.getKeyStroke(KeyEvent.VK_B, 
            KeyEvent.ALT_DOWN_MASK)), null);
    label.setLabelFor(new JButton());
    m = label.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(m.get(KeyStroke.getKeyStroke(KeyEvent.VK_B, 
            KeyEvent.ALT_DOWN_MASK)), "press");  
  }
}
