/* constructors.java -- some checks for the constructors in the JInternalFrame
       class.
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

import javax.swing.JInternalFrame;
import javax.swing.JLayeredPane;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
  }
  
  private void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("()");
    JInternalFrame f = new JInternalFrame();
    harness.check(f.getTitle(), "");
    harness.check(f.getDesktopPane(), null);
    harness.check(f.getDefaultCloseOperation(), 
            JInternalFrame.DISPOSE_ON_CLOSE);
    harness.check(f.getLayer(), JLayeredPane.DEFAULT_LAYER.intValue());
    harness.check(!f.isResizable());
    harness.check(!f.isClosable());
    harness.check(!f.isMaximizable());
    harness.check(!f.isIconifiable());
    f.putClientProperty(JLayeredPane.LAYER_PROPERTY, 
            JLayeredPane.PALETTE_LAYER);
    harness.check(f.getLayer(), JLayeredPane.PALETTE_LAYER.intValue());
    harness.check(f.getClientProperty(JLayeredPane.LAYER_PROPERTY), 
            JLayeredPane.PALETTE_LAYER);
  }

  private void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(String)");
    JInternalFrame f = new JInternalFrame("Title");
    harness.check(f.getTitle(), "Title");
    harness.check(f.getDesktopPane(), null);
    harness.check(f.getDefaultCloseOperation(), 
            JInternalFrame.DISPOSE_ON_CLOSE);
    harness.check(f.getLayer(), JLayeredPane.DEFAULT_LAYER.intValue());
    harness.check(!f.isResizable());
    harness.check(!f.isClosable());
    harness.check(!f.isMaximizable());
    harness.check(!f.isIconifiable());
    
    f = new JInternalFrame(null);
    harness.check(f.getTitle(), null);
  }

  private void testConstructor3(TestHarness harness)
  {
    harness.checkPoint("(String, boolean)");
    JInternalFrame f = new JInternalFrame("Title", true);
    harness.check(f.getTitle(), "Title");
    harness.check(f.getDesktopPane(), null);
    harness.check(f.getDefaultCloseOperation(), 
            JInternalFrame.DISPOSE_ON_CLOSE);
    harness.check(f.getLayer(), JLayeredPane.DEFAULT_LAYER.intValue());
    harness.check(f.isResizable());
    harness.check(!f.isClosable());
    harness.check(!f.isMaximizable());
    harness.check(!f.isIconifiable());

    f = new JInternalFrame(null, false);
    harness.check(f.getTitle(), null);
  }

  private void testConstructor4(TestHarness harness)
  {
    harness.checkPoint("(String, boolean, boolean)");
    JInternalFrame f = new JInternalFrame("Title", false, true);
    harness.check(f.getTitle(), "Title");
    harness.check(f.getDesktopPane(), null);
    harness.check(f.getDefaultCloseOperation(), 
            JInternalFrame.DISPOSE_ON_CLOSE);
    harness.check(f.getLayer(), JLayeredPane.DEFAULT_LAYER.intValue());
    harness.check(!f.isResizable());
    harness.check(f.isClosable());
    harness.check(!f.isMaximizable());
    harness.check(!f.isIconifiable());

    f = new JInternalFrame(null, false, false);
    harness.check(f.getTitle(), null);
  }

  private void testConstructor5(TestHarness harness)
  {
    harness.checkPoint("(String, boolean, boolean, boolean)");
    JInternalFrame f = new JInternalFrame("Title", false, false, true);
    harness.check(f.getTitle(), "Title");
    harness.check(f.getDesktopPane(), null);
    harness.check(f.getDefaultCloseOperation(), 
            JInternalFrame.DISPOSE_ON_CLOSE);
    harness.check(f.getLayer(), JLayeredPane.DEFAULT_LAYER.intValue());
    harness.check(!f.isResizable());
    harness.check(!f.isClosable());
    harness.check(f.isMaximizable());
    harness.check(!f.isIconifiable());

    f = new JInternalFrame(null, false, false, false);
    harness.check(f.getTitle(), null);
  }

  private void testConstructor6(TestHarness harness)
  {
    harness.checkPoint("(String, boolean, boolean, boolean, boolean)");
    JInternalFrame f = new JInternalFrame("Title", false, false, false, true);
    harness.check(f.getTitle(), "Title");
    harness.check(f.getDesktopPane(), null);
    harness.check(f.getDefaultCloseOperation(), 
            JInternalFrame.DISPOSE_ON_CLOSE);
    harness.check(f.getLayer(), JLayeredPane.DEFAULT_LAYER.intValue());
    harness.check(!f.isResizable());
    harness.check(!f.isClosable());
    harness.check(!f.isMaximizable());
    harness.check(f.isIconifiable());

    f = new JInternalFrame(null, false, false, false, false);
    harness.check(f.getTitle(), null);
  }

}
