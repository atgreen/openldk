// Tags: JDK1.4

// Copyright (C) 2011 Pavel Tisnovsky <ptisnovs@redhat.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street,
// Fifth Floor, Boston, MA 02110-1301 USA.

// Tags: GUI, JDK 1.0
// Uses: ../LocationTests

package gnu.testlet.java.awt.GridLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;


/**
 * Check the method {@link GridLayout#minimumLayoutSize(Container)}
 */
public class minimumLayoutSize implements Testlet
{

  public void test(TestHarness harness)
  {
    Container container = new Container();
    GridLayout layout = new GridLayout();

    // The minimum width of a grid layout is the largest minimum width of any
    // of the components in the container times the number of columns, plus the
    // horizontal padding times the number of columns plus one, plus the left
    // and right insets of the target container.
    harness.check(layout.minimumLayoutSize(container), new Dimension(0, 0));
  }

}
