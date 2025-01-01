/* constructors.java -- some checks for the constructor in the LookupTable 
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

// Tags: JDK1.4
// Uses: MyLookupTable

package gnu.testlet.java.awt.image.LookupTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.LookupTable;

public class constructor implements Testlet 
{
  public void test(TestHarness harness)
  {
    LookupTable t = new MyLookupTable(1, 2);
    harness.check(t.getOffset(), 1);
    harness.check(t.getNumComponents(), 2);
    
    // try negative offset
    boolean pass = false;
    try
    {
      t = new MyLookupTable(-1, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // try zero components
    pass = false;
    try
    {
      t = new MyLookupTable(1, 0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
