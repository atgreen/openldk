// Tags: JDK1.2

// Copyright (C) 2006 Robert Schuster <robertschuster@fsfe.org>

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

package gnu.testlet.javax.swing.JTabbedPane;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import javax.swing.JTabbedPane;
import javax.swing.JPanel;
import javax.swing.plaf.UIResource;

public class remove implements Testlet {

    boolean touched_removeTabAt;
    boolean touched_remove;

    public void test(TestHarness harness) {
        JTabbedPane tp = new MyTabbedPane();

        harness.checkPoint("remove");

        // The removal of a component which is an instance of
        // UIResource *should not* provoke a call to
        // JTabbedPane.remove(int) (if it is not a tab
        // component).
        touched_remove = false;
        touched_removeTabAt = false;
        JPanel p = new MyPanel();
        tp.add(p);
        tp.remove(p);
        harness.check(touched_remove, false);
        harness.check(touched_removeTabAt, false);

        // The removal of a component which is an instance of
        // UIResource *should* provoke a call to
        // JTabbedPane.remove(int) if it *is* a tab
        // component.
        touched_remove = false;
        touched_removeTabAt = false;
        p = new MyPanel();
        // The next line makes this UIResource implementing object a tab
        // component although that is normally not done.
        tp.addTab("foo", p);
        tp.remove(p);
        harness.check(touched_remove, false);
        harness.check(touched_removeTabAt, true);

        // The removal of a component which *is not* an instance of
        // UIResource *should* provoke a call to JTabbedPane.remove(int).
        touched_remove = false;
        touched_removeTabAt = false;
        p = new JPanel();
        tp.add(p);
        tp.remove(p);
        harness.check(touched_remove, false);
        harness.check(touched_removeTabAt, true);
    }

    static class MyPanel extends JPanel implements UIResource
    {
    }

    class MyTabbedPane extends JTabbedPane
    {

      public void remove(int i)
      {
        touched_remove = true;
        super.remove(i);
      }

      public void removeTabAt(int i)
      {
        touched_removeTabAt = true;
        super.removeTabAt(i);
      }

    }

}
