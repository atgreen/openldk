/* setInitialDelay.java -- some checks for the setInitialDelay() method in the
       Timer class.
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

// Tags: JDK1.5

package gnu.testlet.javax.swing.Timer;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;

public class setInitialDelay implements Testlet, ActionListener 
{

  public void actionPerformed(ActionEvent event) 
  {
    // ignore      
  }

  public void test(TestHarness harness)
  {
    Timer t = new Timer(100, this);
    t.setInitialDelay(123);
    harness.check(t.getInitialDelay(), 123);
     
    // try zero
    t.setInitialDelay(0);
    harness.check(t.getInitialDelay(), 0);
      
    // try negative
    boolean pass = false;
    try
    {
      t.setInitialDelay(-1);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
