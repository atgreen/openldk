/* testMaximumLayout.java 
   Copyright (C) 2006 Red Hat, 2007 Red Hat
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

// Tags: 1.4

package gnu.testlet.java.awt.CardLayout;

import java.awt.CardLayout;
import java.awt.Dimension;
import java.awt.Frame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testMaximumLayoutSize implements Testlet
{

  public void test(TestHarness harness)
  {
    CardLayout layout = new CardLayout();
    harness.check(layout.maximumLayoutSize(null), 
                  new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE)); 
    
    Frame frame = new Frame();
    harness.check(frame.getComponentCount(), 0);
    harness.check(layout.maximumLayoutSize(frame),
                  new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
  }

}
