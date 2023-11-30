/* getActionForKeyStroke.java -- some checks for the getActionForKeyStroke() 
       method in the JComponent class.
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

public class getActionForKeyStroke implements Testlet
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
    MyJComponent c = new MyJComponent();
    KeyStroke ks = KeyStroke.getKeyStroke('a');
    ActionListener a1 = new ActionListener() 
    {
      public void actionPerformed(ActionEvent e) 
      {
        // ignore
      }  
    };
    ActionListener a2 = new ActionListener() 
    {
      public void actionPerformed(ActionEvent e) 
      {
        // ignore
      }  
    };
    ActionListener a3 = new ActionListener() 
    {
      public void actionPerformed(ActionEvent e) 
      {
        // ignore
      }  
    };
    harness.check(c.getActionForKeyStroke(ks), null);
    c.registerKeyboardAction(a1, ks, JComponent.WHEN_IN_FOCUSED_WINDOW);
    harness.check(c.getActionForKeyStroke(ks), a1);
    c.registerKeyboardAction(a2, ks, 
            JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    harness.check(c.getActionForKeyStroke(ks), a2);
    c.registerKeyboardAction(a3, ks, JComponent.WHEN_FOCUSED);
    harness.check(c.getActionForKeyStroke(ks), a3);
    
    harness.check(c.getActionForKeyStroke(null), null);
  }
}
