// Tags: JDK1.2

// Copyright (C) 2004 Thomas Zander <zander@kde.org>

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

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.awt.*;
import javax.swing.*;

/**
 * These tests pass with the Sun JDK 1.4.1_03 on GNU/Linux IA-32.
 */
public class Icon implements Testlet {
    public void test(TestHarness harness) {
        MyIcon icon = new MyIcon();
        JLabel l = new JLabel(icon);
        harness.check(l.getIcon(), icon);
        harness.check(l.getDisabledIcon(), null);
        l.setIcon(icon);
        harness.check(l.getIcon(), icon);
        harness.check(l.getDisabledIcon(), null);

        l = new JLabel();
        Dimension base = l.getPreferredSize();
        l.setIcon(icon);
        Dimension one = l.getPreferredSize();
        harness.check(one.width, base.width + icon.getIconWidth());

        l = new JLabel("bla");
        base = l.getPreferredSize();
        l.setIcon(icon);
        one = l.getPreferredSize();
        harness.check(one.width, base.width + icon.getIconWidth() + l.getIconTextGap() );

        l.setIconTextGap(100);
        one = l.getPreferredSize();
        harness.check(one.width, base.width + icon.getIconWidth() + 100 );
    }

    private static class MyIcon implements javax.swing.Icon {
        private int painted = 0;
        public int getIconHeight()  {
            return 10;
        }
        public int getIconWidth()  {
            return 20;
        }
        public void paintIcon(Component c, Graphics g, int x, int y)  {
            painted++;
        }
        public int getPainted() {
            return painted;
        }
    }
}
