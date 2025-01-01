/* defaultProperties.java
   Copyright (C) 2006 Red Hat
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

// Tags: 1.4

package gnu.testlet.java.awt.FileDialog;

import java.awt.FileDialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class defaultProperties implements Testlet
{

  public void test(TestHarness harness)
  {
    // This test ensures that all properties are set
    // to the default values.
    FileDialog dialog = new FileDialog(new Frame());
    harness.check(dialog.getAlignmentX(), 0.5);
    harness.check(dialog.getAlignmentY(), 0.5);
    harness.check(dialog.getComponentCount(), 0);
    harness.check(dialog.getFocusableWindowState(), true);
    harness.check(dialog.getFocusTraversalKeysEnabled(), true);
    harness.check(dialog.getHeight(), 0);
    harness.check(dialog.getIgnoreRepaint(), false);
    harness.check(dialog.getWidth(), 0);
    harness.check(dialog.getX(), 0);
    harness.check(dialog.getY(), 0);
    harness.check(dialog.getBackground(), null);
    harness.check(dialog.getBounds(), new Rectangle());
    harness.check(dialog.getDropTarget(), null);
    harness.check(dialog.getFocusCycleRootAncestor(), null);
    harness.check(dialog.getFocusOwner(), null);
    harness.check(dialog.getFont(), null);
    harness.check(dialog.getForeground(), null);
    harness.check(dialog.getGraphics(), null);
    harness.check(dialog.getInputMethodRequests(), null);
    harness.check(dialog.getInsets(), new Insets(0, 0, 0, 0));
    harness.check(dialog.getLayout(), null);
    harness.check(dialog.getLocation(), new Point());
    harness.check(dialog.getMaximumSize(), 
                       new Dimension(32767, 32767));
    harness.check(dialog.getMinimumSize(), new Dimension());
    harness.check(dialog.getName(), "filedlg0");
    harness.check(dialog.getPreferredSize(), new Dimension());
    harness.check(dialog.getSize(), new Dimension());
    harness.check(dialog.getTitle(), "");
    harness.check(dialog.getWarningString(), null);
    
    harness.check(dialog.isActive(), false);
    harness.check(dialog.isAlwaysOnTop(), false);
    harness.check(dialog.isBackgroundSet(), false);
    harness.check(dialog.isCursorSet(), true);
    harness.check(dialog.isDisplayable(), false);
    harness.check(dialog.isDoubleBuffered(), false);
    harness.check(dialog.isEnabled(), true);
    harness.check(dialog.isFocusable(), true);
    harness.check(dialog.isFocusableWindow(), true);
    harness.check(dialog.isFocusCycleRoot(), true);
    harness.check(dialog.isFocused(), false);
    harness.check(dialog.isFocusOwner(), false);
    harness.check(dialog.isFocusTraversalPolicyProvider(), false);
    harness.check(dialog.isFocusTraversalPolicySet(), true);
    harness.check(dialog.isFontSet(), false);
    harness.check(dialog.isForegroundSet(), false);
    harness.check(dialog.isLightweight(), false);
    harness.check(dialog.isMaximumSizeSet(), false);
    harness.check(dialog.isMinimumSizeSet(), false);
    harness.check(dialog.isModal(), true);
    harness.check(dialog.isOpaque(), false); // Currently fails on Classpath
    harness.check(dialog.isPreferredSizeSet(), false);
    harness.check(dialog.isResizable(), true);
    harness.check(dialog.isShowing(), false);
    harness.check(dialog.isUndecorated(), false);
    harness.check(dialog.isValid(), false);
    harness.check(dialog.isVisible(), false);
    
  }

}
