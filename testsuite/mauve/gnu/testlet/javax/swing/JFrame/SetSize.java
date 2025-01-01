// Tags: JDK1.2 GUI

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

package gnu.testlet.javax.swing.JFrame;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.JFrame;
import javax.swing.JComponent;
import java.awt.*;
import java.util.Vector;

/**
 * These tests pass with the Sun JDK 1.4.2_05 on GNU/Linux IA-32.
 */
public class SetSize implements Testlet {
    public void test(TestHarness harness) {
        JFrame f = new JFrame();
        f.setSize(new Dimension(170, 300));
        f.setVisible(true);

        try {
            Thread.sleep(200); // needed for Suns VM...
        } catch(InterruptedException e) {
            return;
        }
        Insets i = f.getInsets();
        int w = f.getContentPane().getWidth();
        int h = f.getContentPane().getHeight();
        int x = f.getContentPane().getX();
        int y = f.getContentPane().getY();
        f.setVisible(false);
        f.dispose();
        // The request of sizing was 170x300, lets see if we managed that.
        harness.check((w + i.left + i.right) == 170);
        harness.check((h + i.top + i.bottom) == 300);

        // The contentPane is per definition set to 0, 0
        harness.check(x == 0);
        harness.check(y == 0);

        // Content pane Painting Size
        f = new JFrame();
        f.setSize(new Dimension(170, 300));
        final Vector vars = new Vector(1);
        JComponent child = new JComponent() {
            public void paint(Graphics g) {
                if(g.getClip() != null) {
                    vars.add(g.getClip());
                    synchronized(vars) {
                        vars.notifyAll();
                    }
                }
            }
        };
        f.getContentPane().add(child);
        f.setVisible(true);// technically, this statement should be in the sync block, but that only works on the Sun JVM..
        synchronized(vars) {
            try {
                vars.wait(1000); // wait until it has been painted, for max of 1 sec.
            } catch(InterruptedException e) {
                // fail
            } finally {
                w = f.getContentPane().getWidth();
                h = f.getContentPane().getHeight();
                f.setVisible(false);
                f.dispose();
            }
        }
        Rectangle r = (Rectangle) vars.get(0);
        harness.check(r != null);
        harness.check(w == r.getWidth());
        harness.check(h == r.getHeight());
    }
}
