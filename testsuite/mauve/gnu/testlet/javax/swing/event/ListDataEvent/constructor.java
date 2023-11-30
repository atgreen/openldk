/* constructor.java -- some checks for the constructor in the ListDataEvent 
       class.
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.event.ListDataEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.event.ListDataEvent;

public class constructor implements Testlet
{
  public void test(TestHarness harness)
  {
    ListDataEvent e = new ListDataEvent(this, ListDataEvent.CONTENTS_CHANGED, 
            1, 2);
    harness.check(e.getSource(), this);
    harness.check(e.getType(), ListDataEvent.CONTENTS_CHANGED);
    harness.check(e.getIndex0(), 1);
    harness.check(e.getIndex1(), 2);
    
    // try null source
    boolean pass = false;
    try
    {
      e = new ListDataEvent(null, ListDataEvent.CONTENTS_CHANGED, 1, 2);
    }
    catch (IllegalArgumentException ex)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try first index greater than second
    e = new ListDataEvent(this, ListDataEvent.CONTENTS_CHANGED, 99, 2);
    harness.check(e.getIndex0(), 2);
    harness.check(e.getIndex1(), 99);
    
    // try bad type
    e = new ListDataEvent(this, -99, 1, 2);
    harness.check(e.getType(), -99);
    
    // try negative first index
    e = new ListDataEvent(this, -ListDataEvent.CONTENTS_CHANGED, -10, 2);
    harness.check(e.getIndex0(), -10);
    harness.check(e.getIndex1(), 2);
    
    // try negative second index
    e = new ListDataEvent(this, -ListDataEvent.CONTENTS_CHANGED, 1, -2);
    harness.check(e.getIndex0(), -2);
    harness.check(e.getIndex1(), 1);
  }
}
