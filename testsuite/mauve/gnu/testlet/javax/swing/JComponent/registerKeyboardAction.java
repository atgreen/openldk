/* registerKeyboardAction.java -- some checks for the registerKeyboardAction()
      methods in the JComponent class.
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

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComponent;
import javax.swing.KeyStroke;

public class registerKeyboardAction implements Testlet
{
  static class MyJComponent extends JComponent
  {
    public MyJComponent()
    {
      super();
    }
  }
  
  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }

  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(ActionListener, String, KeyStroke, int)");
    JComponent c = new MyJComponent();
    ActionListener l1 = new ActionListener() 
    {
      public void actionPerformed(ActionEvent e) 
      {
        // ignore
      }   
    };
    c.registerKeyboardAction(l1, "ABC", KeyStroke.getKeyStroke('a'), 
            JComponent.WHEN_FOCUSED);
    harness.check(c.getInputMap(JComponent.WHEN_FOCUSED).keys()[0], 
            KeyStroke.getKeyStroke('a'));
    Object link = c.getInputMap(JComponent.WHEN_FOCUSED).get(
            KeyStroke.getKeyStroke('a'));
    harness.check(c.getActionMap().get(link), link);
  }
  
  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(ActionListener, KeyStroke, int)");    
    JComponent c = new MyJComponent();
    ActionListener l1 = new ActionListener() 
    {
      public void actionPerformed(ActionEvent e) 
      {
        // ignore
      }   
    };
    c.registerKeyboardAction(l1, KeyStroke.getKeyStroke('a'), 
            JComponent.WHEN_FOCUSED);
    harness.check(c.getInputMap(JComponent.WHEN_FOCUSED).keys()[0], 
            KeyStroke.getKeyStroke('a'));
    Object link = c.getInputMap(JComponent.WHEN_FOCUSED).get(
            KeyStroke.getKeyStroke('a'));
    harness.check(c.getActionMap().get(link), link);
  }

}
