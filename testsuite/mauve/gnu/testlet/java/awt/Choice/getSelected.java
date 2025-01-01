// Tags: JDK1.1

// Copyright (C) 2004 Free Software Foundation
// Contributed by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.awt.Choice;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.awt.*;

public class getSelected implements Testlet
{
  public void test(TestHarness harness)
  {
    harness.checkPoint("new");
    Choice c = new Choice();
    harness.check(c.countItems(), 0);
    harness.check(c.getItemCount(), 0);
    harness.check(c.getSelectedIndex(), -1);
    harness.check(c.getSelectedItem(), null);
    harness.check(c.getSelectedObjects(), null);

    harness.checkPoint("add");
    String gnu = "GNU";
    c.add(gnu);
    harness.check(c.countItems(), 1);
    harness.check(c.getItemCount(), 1);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getSelectedItem(), gnu);
    Object[] objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(gnu));

    harness.checkPoint("addItem");
    String fsf = "FSF";
    c.add(fsf);
    harness.check(c.countItems(), 2);
    harness.check(c.getItemCount(), 2);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getSelectedItem(), gnu);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(gnu));

    harness.checkPoint("removeAll");
    c.removeAll();
    harness.check(c.countItems(), 0);
    harness.check(c.getItemCount(), 0);
    harness.check(c.getSelectedIndex(), -1);
    harness.check(c.getSelectedItem(), null);
    harness.check(c.getSelectedObjects(), null);

    harness.checkPoint("insert-mauve");
    String mauve = "Mauve";
    c.insert(mauve, 0);
    harness.check(c.countItems(), 1);
    harness.check(c.getItemCount(), 1);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getSelectedItem(), mauve);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(mauve));

    harness.checkPoint("insert-gnu");
    c.insert(gnu, 1);
    harness.check(c.countItems(), 2);
    harness.check(c.getItemCount(), 2);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getSelectedItem(), mauve);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(mauve));

    harness.checkPoint("select-1");
    c.select(1);
    harness.check(c.countItems(), 2);
    harness.check(c.getItemCount(), 2);
    harness.check(c.getSelectedIndex(), 1);
    harness.check(c.getSelectedItem(), gnu);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(gnu));

    harness.checkPoint("insert-fsf");
    c.insert(fsf, 1);
    harness.check(c.countItems(), 3);
    harness.check(c.getItemCount(), 3);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getSelectedItem(), mauve);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(mauve));

    harness.checkPoint("select-gnu");
    c.select(new String(gnu)); // Create a different fsf string
    harness.check(c.countItems(), 3);
    harness.check(c.getItemCount(), 3);
    harness.check(c.getSelectedIndex(), 2);
    harness.check(c.getSelectedItem(), gnu);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(gnu));

    harness.checkPoint("insert classpath");
    String classpath = "classpath";
    c.insert(classpath, 9);
    harness.check(c.countItems(), 4);
    harness.check(c.getItemCount(), 4);
    harness.check(c.getSelectedIndex(), 2);
    harness.check(c.getSelectedItem(), gnu);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(gnu));

    harness.checkPoint("add-remove-dummy");
    c.add("dummy");
    c.remove("dummy");
    harness.check(c.countItems(), 4);
    harness.check(c.getItemCount(), 4);
    harness.check(c.getSelectedIndex(), 2);
    harness.check(c.getSelectedItem(), gnu);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(gnu));

    harness.checkPoint("remove-0");
    c.remove(0);
    harness.check(c.countItems(), 3);
    harness.check(c.getItemCount(), 3);
    harness.check(c.getSelectedIndex(), 1);
    harness.check(c.getSelectedItem(), gnu);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(gnu));

    harness.checkPoint("remove-gnu");
    c.remove(new String(gnu));
    harness.check(c.countItems(), 2);
    harness.check(c.getItemCount(), 2);
    harness.check(c.getSelectedIndex(), 0);
    harness.check(c.getSelectedItem(), fsf);
    objects = c.getSelectedObjects();
    harness.check(objects != null && objects.length == 1);
    harness.check(objects != null && objects[0].equals(fsf));

    harness.checkPoint("remove-1-remove-0");
    c.remove(1);
    c.remove(0);
    harness.check(c.countItems(), 0);
    harness.check(c.getItemCount(), 0);
    harness.check(c.getSelectedIndex(), -1);
    harness.check(c.getSelectedItem(), null);
    harness.check(c.getSelectedObjects(), null);
  }
}
