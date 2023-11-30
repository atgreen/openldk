// Tags: JDK1.2

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

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Graphics;
import javax.swing.JComponent;
import javax.swing.JFrame;

/**
 * Tests if the paint() method works correctly.
 */
public class paint implements Testlet 
{
  StringBuffer callOrder = new StringBuffer();
  class TestComponent extends JComponent
  {
    protected void paintComponent(Graphics g)
    {
      callOrder.append('1');
    }
    protected void paintBorder(Graphics g)
    {
      callOrder.append('2');
    }
    protected void paintChildren(Graphics g)
    {
      callOrder.append('3');
    }
  }

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    JFrame f = new JFrame();
    JComponent c = new TestComponent();
    f.setContentPane(c);
    f.setSize(100, 100);
    f.setVisible(true);
    callOrder.delete(0, callOrder.length());
    Graphics g = c.getGraphics();
    c.paint(g);
    // If the components receives multiple paint requests (like the system
    // triggers a repaint), then we might get 123123123 or something. To avoid
    // trouble like this, we check using startsWith().
    harness.check(callOrder.toString().startsWith("123"));
    f.dispose();
  }
}
