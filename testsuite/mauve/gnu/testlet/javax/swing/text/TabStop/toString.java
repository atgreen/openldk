/* toString.java -- some checks for the toString() method in the TabStop class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.text.TabStop;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.TabStop;

public class toString implements Testlet
{
  public void test(TestHarness harness)
  {
    TabStop ts1 = new TabStop(1.0f);
    harness.check(ts1.toString(), "tab @1.0");
    ts1 = new TabStop(2.0f, TabStop.ALIGN_RIGHT, TabStop.LEAD_EQUALS);
    harness.check(ts1.toString(), "right tab @2.0 (w/leaders)");
    ts1 = new TabStop(10.999f, TabStop.ALIGN_CENTER, TabStop.LEAD_HYPHENS);
    harness.check(ts1.toString(), "center tab @10.999 (w/leaders)");
    ts1 = new TabStop(3.3f, TabStop.ALIGN_BAR, TabStop.LEAD_NONE);
    harness.check(ts1.toString(), "bar tab @3.3");
    ts1 = new TabStop(3.3f, TabStop.ALIGN_DECIMAL, TabStop.LEAD_UNDERLINE);
    harness.check(ts1.toString(), "decimal tab @3.3 (w/leaders)");
    ts1 = new TabStop(3.3f, 99, 666);
    harness.check(ts1.toString(), "tab @3.3 (w/leaders)");
  }
}
