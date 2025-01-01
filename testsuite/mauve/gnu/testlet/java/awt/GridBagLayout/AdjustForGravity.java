// Tags: GUI JDK1.0

// Copyright (C) 2006 Red Hat

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
// along with GNU Classpath; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.java.awt.GridBagLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.java.awt.LocationTests;

import java.awt.*;

/**
 * Test GridBagLayout.AdjustForGravity.
 */
public class AdjustForGravity implements Testlet
{
  public class MyLayout extends GridBagLayout
  {
    public void AdjustForGravity(GridBagConstraints c, Rectangle r)
    {
      super.AdjustForGravity(c, r);
    }
  }

  public void test(TestHarness h)
  {
    MyLayout l = new MyLayout();
    GridBagConstraints c = new GridBagConstraints();
    Rectangle r = new Rectangle(4, 4, 10, 10);

    // Check fill values.
    Rectangle[] fillRects =
      {
        new Rectangle(9, 9, 0, 0),
        new Rectangle(4, 4, 10, 10),
        new Rectangle(4, 9, 10, 0),
        new Rectangle(9, 4, 0, 10)
      };

    for (int i = GridBagConstraints.NONE;
         i <= GridBagConstraints.VERTICAL;
         i++)
      {
        r = new Rectangle(4, 4, 10, 10);
        c.fill = i;
        l.AdjustForGravity(c, r);
        h.check(r.equals(fillRects[i - GridBagConstraints.NONE]));
      }

    c.fill = GridBagConstraints.NONE;

    // Check anchor values.
    Rectangle[] anchorRects =
      {
        new Rectangle(9, 9, 0, 0),
        new Rectangle(9, 4, 0, 0),
        new Rectangle(14, 4, 0, 0),
        new Rectangle(14, 9, 0, 0),
        new Rectangle(14, 14, 0, 0),
        new Rectangle(9, 14, 0, 0),
        new Rectangle(4, 14, 0, 0),
        new Rectangle(4, 9, 0, 0),
        new Rectangle(4, 4, 0, 0)
      };

    for (int i = GridBagConstraints.CENTER;
         i <= GridBagConstraints.NORTHWEST;
         i++)
      {
        r = new Rectangle(4, 4, 10, 10);
        c.anchor = i;
        l.AdjustForGravity(c, r);
        h.check(r.equals(anchorRects[i - GridBagConstraints.CENTER]));
      }

    // Check inset values.
    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(1, 4, 3, 0);
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(11, 8, 0, 0)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0, 4, 0, 1);
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(10, 9, 0, 0)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(2, 4, 3, 2);
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(10, 8, 0, 0)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(3, 4, 3, 3);
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(9, 9, 0, 0)));

    // Check ipad values.
    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 1;
    c.ipady = 4;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(8, 7, 1, 4)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 2;
    c.ipady = 3;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(8, 7, 2, 3)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 3;
    c.ipady = 2;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(7, 8, 3, 2)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 4;
    c.ipady = 1;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(7, 8, 4, 1)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 1;
    c.ipady = 4;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(8, 4, 1, 10)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 2;
    c.ipady = 3;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(8, 4, 2, 10)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 3;
    c.ipady = 2;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(7, 4, 3, 10)));

    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.VERTICAL;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 4;
    c.ipady = 1;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(7, 4, 4, 10)));

    // Check weight values.
    c.anchor = GridBagConstraints.CENTER;
    c.fill = GridBagConstraints.NONE;
    c.insets = new Insets(0, 0, 0, 0);
    c.ipadx = 0;
    c.ipady = 0;
    c.weightx = 0.3;
    c.weighty = 0.3;
    r = new Rectangle(4, 4, 10, 10);
    l.AdjustForGravity(c, r);
    h.check(r.equals(new Rectangle(9, 9, 0, 0)));

    // Check a range of fill and anchor values.
    // gridwidth and gridheight have no effect.
    c.gridwidth = 0;
    c.gridheight = 0;
    // gridx and gridy have no effect.
    c.gridx = 0;
    c.gridy = 0;
    // weightx and weighty have no effect.
    c.weightx = 0;
    c.weighty = 0;

    c.ipadx = 1;
    c.ipady = 2;
    c.insets.top = 3;
    c.insets.right = 2;
    c.insets.bottom = 1;
    c.insets.left = 0;

    Rectangle[] allRects =
      {
        new Rectangle(40, 28, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 28, 71, 2),
        new Rectangle(40, 9, 1, 40),
        new Rectangle(40, 9, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 9, 71, 2),
        new Rectangle(40, 9, 1, 40),
        new Rectangle(75, 9, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 9, 71, 2),
        new Rectangle(75, 9, 1, 40),
        new Rectangle(75, 28, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 28, 71, 2),
        new Rectangle(75, 9, 1, 40),
        new Rectangle(75, 47, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 47, 71, 2),
        new Rectangle(75, 9, 1, 40),
        new Rectangle(40, 47, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 47, 71, 2),
        new Rectangle(40, 9, 1, 40),
        new Rectangle(5, 47, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 47, 71, 2),
        new Rectangle(5, 9, 1, 40),
        new Rectangle(5, 28, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 28, 71, 2),
        new Rectangle(5, 9, 1, 40),
        new Rectangle(5, 9, 1, 2),
        new Rectangle(5, 9, 71, 40),
        new Rectangle(5, 9, 71, 2),
        new Rectangle(5, 9, 1, 40)
      };

    int i = 0;
    for (int anchor = GridBagConstraints.CENTER; anchor <= GridBagConstraints.NORTHWEST; anchor++)
      {
        for (int fill = GridBagConstraints.NONE; fill <= GridBagConstraints.VERTICAL; fill++)
          {
            c.anchor = anchor;
            c.fill = fill;

            r.x = 5;
            r.y = 6;
            r.width = 73;
            r.height = 44;

            l.AdjustForGravity(c, r);
            h.check(r.equals(allRects[i]));
            i++;
          }
      }
  }

  public void printConstraints(GridBagConstraints c)
  {
    System.out.println("fill = " + c.fill);
    System.out.println("anchor = " + c.anchor);
    System.out.println("gridheight = " + c.gridheight);
    System.out.println("gridwidth = " + c.gridwidth);
    System.out.println("gridx = " + c.gridx);
    System.out.println("gridy = " + c.gridy);
    System.out.println("insets = " + c.insets);
    System.out.println("ipadx = " + c.ipadx);
    System.out.println("ipady = " + c.ipady);
    System.out.println("weightx = " + c.weightx);
    System.out.println("weighty = " + c.weighty);
  }

  public void printRectangle(Rectangle r)
  {
    System.out.println("new Rectangle("
                       + r.x + ", "
                       + r.y + ", "
                       + r.width + ", "
                       + r.height + "),");
  }
}
