// Tags: JDK1.2
// Uses: TestLabel

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.swing.JLabel;

public class setText implements Testlet, PropertyChangeListener
{
  List events = new java.util.ArrayList();

  public void propertyChange(PropertyChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness)
  {
    testGeneral(harness);
    testGeneralWithMnemonic(harness);
    testRepaint(harness);
    testRevalidate(harness);
  }
  
  public void testGeneral(TestHarness harness)
  {
    harness.checkPoint("testGeneral()");
    
    // simple test 
    JLabel label = new JLabel("ABC");
    label.addPropertyChangeListener(this);
    label.setText("XYZ");
    harness.check(label.getText(), "XYZ");
    harness.check(events.size(), 1);
    PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "text");
    harness.check(e0.getOldValue(), "ABC");
    harness.check(e0.getNewValue(), "XYZ");

    // setting the same again should generate no event
    events.clear();
    label.setText("XYZ");
    harness.check(events.size(), 0);
    
    // check null
    events.clear();
    label.setText(null);
    harness.check(label.getText(), null);
    harness.check(events.size(), 1);
    e0 = (PropertyChangeEvent) events.get(0);
    harness.check(e0.getSource(), label);
    harness.check(e0.getPropertyName(), "text");
    harness.check(e0.getOldValue(), "XYZ");
    harness.check(e0.getNewValue(), null);
    
    // setting the same again should generate no event
    events.clear();
    label.setText(null);
    harness.check(events.size(), 0);
  }

  public void testGeneralWithMnemonic(TestHarness harness)
  {
      harness.checkPoint("testGeneralWithMnemonic()");
      
      // simple test 
      JLabel label = new JLabel("ABC");
      label.setDisplayedMnemonic('C');
      harness.check(label.getDisplayedMnemonicIndex(), 2);
      events.clear();
      label.addPropertyChangeListener(this);
      label.setText("CAB");
      harness.check(label.getText(), "CAB");
      harness.check(label.getDisplayedMnemonicIndex(), 0);
      harness.check(events.size(), 2);
      PropertyChangeEvent e0 = (PropertyChangeEvent) events.get(0);
      harness.check(e0.getSource(), label);
      harness.check(e0.getPropertyName(), "text");
      harness.check(e0.getOldValue(), "ABC");
      harness.check(e0.getNewValue(), "CAB");

      PropertyChangeEvent e1 = (PropertyChangeEvent) events.get(1);
      if (e1.getPropertyName().equals("html"))
          e1 = (PropertyChangeEvent) events.get(2);
      harness.check(e1.getSource(), label);
      harness.check(e1.getPropertyName(), "displayedMnemonicIndex");
      harness.check(e1.getOldValue(), new Integer(2));
      harness.check(e1.getNewValue(), new Integer(0));
      
      // setting the same again should generate no event
      events.clear();
      label.setText("CAB");
      harness.check(events.size(), 0);
      
      // check null
      events.clear();
      label.setText(null);
      harness.check(label.getText(), null);
      harness.check(label.getDisplayedMnemonicIndex(), -1);
      harness.check(events.size(), 2);
      e0 = (PropertyChangeEvent) events.get(0);
      harness.check(e0.getSource(), label);
      harness.check(e0.getPropertyName(), "text");
      harness.check(e0.getOldValue(), "CAB");
      harness.check(e0.getNewValue(), null);

      e1 = (PropertyChangeEvent) events.get(1);
      if (e1.getPropertyName().equals("html"))
          e1 = (PropertyChangeEvent) events.get(2);
      harness.check(e1.getSource(), label);
      harness.check(e1.getPropertyName(), "displayedMnemonicIndex");
      harness.check(e1.getOldValue(), new Integer(0));
      harness.check(e1.getNewValue(), new Integer(-1));

  }
  
  /**
   * Tests if setText triggers a repaint. setText should trigger a
   * repaint() call whenever the actual value of the property changes.
   *
   * @param harness the test harness to use
   */
  private void testRepaint(TestHarness harness)
  {
    TestLabel c = new TestLabel();
    // Set to null, so that we know the state.
    c.setText(null);
    c.repaintCalled = false;
    // Change state and check if repaint is called.
    c.setText("Test1");
    harness.check(c.repaintCalled, true);
    // Don't change state.
    c.repaintCalled = false;
    c.setText("Test1");
    harness.check(c.repaintCalled, false);
    // Change state and check if repaint is called.
    c.repaintCalled = false;
    c.setText("Text2");
    harness.check(c.repaintCalled, true);
    // Don't change state.
    c.repaintCalled = false;
    c.setText("Text2");
    harness.check(c.repaintCalled, false);
  }

  /**
   * Tests if setEnabled triggers a revalidate.
   *
   * @param harness the test harness to use
   */
  private void testRevalidate(TestHarness harness)
  {
    TestLabel c = new TestLabel();
    // Set to false, so that we know the state.
    c.setText(null);
    c.revalidateCalled = false;
    // Change state and check if repaint is called.
    c.setText("Test1");
    harness.check(c.revalidateCalled, true);
    // Don't change state.
    c.revalidateCalled = false;
    c.setText("Test1");
    harness.check(c.revalidateCalled, false);
    // Change state and check if repaint is called.
    c.revalidateCalled = false;
    c.setText("Test2");
    harness.check(c.revalidateCalled, true);
    // Don't change state.
    c.revalidateCalled = false;
    c.setText("Test2");
    harness.check(c.revalidateCalled, false);
  }

}
