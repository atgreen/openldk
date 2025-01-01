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

package gnu.testlet.java.awt.CardLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.CardLayout;

// test of method toString()
public class toString
  implements Testlet
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    // test constructor without any parameter
    CardLayout cardLayout1 = new CardLayout();
    cardLayout1.toString();
    harness.check(cardLayout1.toString() != null);
    harness.check(cardLayout1.toString(), "java.awt.CardLayout[hgap=0,vgap=0]");

    // test constructor with two parameters
    CardLayout cardLayout2 = new CardLayout(50, 50);
    cardLayout2.toString();
    harness.check(cardLayout1.toString() != null);
    harness.check(cardLayout2.toString(), "java.awt.CardLayout[hgap=50,vgap=50]");
  }
}

