/* minimumLayoutSize.java 
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

// Tags: JDK1.0

package gnu.testlet.java.awt.FlowLayout;

import java.awt.Button;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.List;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class minimumLayoutSize implements Testlet
{

  public void test(TestHarness harness)
  {
    Container container = new Container();
    FlowLayout layout = new FlowLayout();

    // Show that the width is calculated using the formula:
    //    w += 2 * hgap + ins.left + ins.right
    // when there are no components in container.
    harness.check(layout.minimumLayoutSize(container), new Dimension(10, 10));
    
    // Show that the width is calculated using the formula:
    //   w += (num + 1) * hgap + ins.left + ins.right;
    // when there is one or more components in container.
    container.add(new Button());
    harness.check(layout.minimumLayoutSize(container), new Dimension(10, 10));
    container.add(new Button());
    container.add(new List());
    harness.check(layout.minimumLayoutSize(container), new Dimension(20, 10));
  }

}
