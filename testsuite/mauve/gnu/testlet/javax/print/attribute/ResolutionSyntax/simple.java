//Tags: JDK1.4

//Copyright (C) 2005 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 51 Franklin Street, Fifth Floor, 
//Boston, MA 02110-1301 USA.

package gnu.testlet.javax.print.attribute.ResolutionSyntax;

import javax.print.attribute.ResolutionSyntax;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the correct conversions of the resolutions
 * between different units, lessThanOrEqual and
 * equals methods.
 */
public class simple implements Testlet
{
  // Helper class to make abstract class usable.  
  public class TestResolutionSyntax extends ResolutionSyntax 
  {
    public TestResolutionSyntax(int crossFeedResolution, 
                                int feedResolution, int units)
    {
      super(crossFeedResolution, feedResolution, units);
    }   
    
  }
  public void test(TestHarness harness)
  {
    TestResolutionSyntax test = 
      new TestResolutionSyntax(400, 600, ResolutionSyntax.DPI);
    
    harness.checkPoint("toString");
    harness.check(test.toString(), "40000x60000 dphi");
    harness.check(test.toString(ResolutionSyntax.DPI, "dpi"), "400x600 dpi");
    harness.check(test.toString(ResolutionSyntax.DPCM, "dpcm"), 
                  "157x236 dpcm");
    harness.check(test.toString(ResolutionSyntax.DPCM, null), "157x236");
    
    harness.checkPoint("getFeedResolution");
    harness.check(test.getFeedResolution(ResolutionSyntax.DPCM), 236);
    harness.check(test.getFeedResolution(ResolutionSyntax.DPI), 600);    

    TestResolutionSyntax test2 = 
      new TestResolutionSyntax(400, 600, ResolutionSyntax.DPI);
    TestResolutionSyntax test3 = 
      new TestResolutionSyntax(401, 610, ResolutionSyntax.DPI);
    TestResolutionSyntax test4 = 
      new TestResolutionSyntax(389, 589, ResolutionSyntax.DPI);
    
    harness.checkPoint("lessThanOrEqual");
    harness.check(test.lessThanOrEquals(test2), true);
    harness.check(test.lessThanOrEquals(test3), true);
    harness.check(test.lessThanOrEquals(test4), false);

    TestResolutionSyntax test5 = new TestResolutionSyntax(40000, 60000, 1);
    
    harness.checkPoint("equals");
    harness.check(test.equals(test2), true);
    harness.check(test.equals(test5), true);
    harness.check(test3.equals(test5), false);
  }

}
