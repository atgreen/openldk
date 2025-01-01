/* PR23996.java -- test for bug PR23996
   Copyright (C) 2006 Mario Torre
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

package gnu.testlet.java.text.DecimalFormat;

import java.text.DecimalFormat;
import java.util.Locale;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This is based on PR23996: DecimalFormat.format() is giving
 * different values in Java and DOTNet.
 * 
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class PR23996 implements Testlet 
{
  public void test(TestHarness harness)
  {
    Locale orig = Locale.getDefault();
    Locale.setDefault(Locale.US);
    
    harness.checkPoint("PR23996");
    
    DecimalFormat df = new DecimalFormat("S#.12345");
    harness.check(df.format(Float.MAX_VALUE),
                  "S340282346638528860000000000000000000000.12345");
    
    DecimalFormat df1 = new DecimalFormat("S#.00");
    harness.check(df1.format(Float.MAX_VALUE),
                  "S340282346638528860000000000000000000000.00");

    DecimalFormat df2 = new DecimalFormat("0.7547895");
    harness.check(df2.format(Float.MAX_VALUE),
                  "340282346638528860000000000000000000000.7547895");
    
    Locale.setDefault(orig);
  }
}
