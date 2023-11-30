// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <roman@kennke.org>

// This file is part of Mauve.

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.JToolBar;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JToolBar;
import javax.swing.plaf.BorderUIResource;

/**
 * Checks if buttons that are added to a JToolBar have their insets
 * modified correctly.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class buttonInsets implements Testlet
{
  public void test(TestHarness h)
  {
    h.checkPoint("JMenu");
    JToolBar tb = new JToolBar();
    JButton button = new JButton("test");

    Insets insets = button.getInsets();
    h.check(insets.top, 5, "insets.top: " + insets.top);
    h.check(insets.bottom, 5, "insets.bottom: " + insets.bottom);
    h.check(insets.left, 17, "insets.left: " + insets.left);
    h.check(insets.right, 17, "insets.right: " + insets.right);

    Insets margin = button.getMargin();
    h.check(margin.top, 2, "insets.top: " + margin.top);
    h.check(margin.bottom, 2, "insets.bottom: " + margin.bottom);
    h.check(margin.left, 14, "insets.left: " + margin.left);
    h.check(margin.right, 14, "insets.right: " + margin.right);

    tb.add(button);

    insets = button.getInsets();

    h.check(insets.top, 6, "insets.top: " + insets.top);
    h.check(insets.bottom, 6, "insets.bottom: " + insets.bottom);
    h.check(insets.left, 6, "insets.left: " + insets.left);
    h.check(insets.right, 6, "insets.right: " + insets.right);

    margin = button.getMargin();
    h.check(margin.top, 2, "insets.top: " + margin.top);
    h.check(margin.bottom, 2, "insets.bottom: " + margin.bottom);
    h.check(margin.left, 14, "insets.left: " + margin.left);
    h.check(margin.right, 14, "insets.right: " + margin.right);

  }

}
