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
//Boston, MA, 02110-1301 USA.

package gnu.testlet.javax.print.attribute.Size2DSyntax;

import javax.print.attribute.Size2DSyntax;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/** 
 * Tests all the correct unit conversions through
 * the toString methods.
 */
public class simple implements Testlet
{
  // Test class making abstract class to test usable.
  public class TestSize2DSyntax extends Size2DSyntax
  {
    public TestSize2DSyntax(float x, float y, int units)
    {
      super(x, y, units);
    }

    public TestSize2DSyntax(int x, int y, int units)
    {
      super(x, y, units);
    }    
  }

  public void test(TestHarness harness)
  {
    TestSize2DSyntax floatInch = 
      new TestSize2DSyntax(55.6f, 232.1f, Size2DSyntax.INCH);
    TestSize2DSyntax intInch = new TestSize2DSyntax(8, 12, Size2DSyntax.INCH);
    TestSize2DSyntax floatMM = 
      new TestSize2DSyntax(55.6f, 232.1f, Size2DSyntax.MM);
    TestSize2DSyntax intMM = new TestSize2DSyntax(210, 297, Size2DSyntax.MM);
       
    harness.checkPoint("units conversions/toString");
    harness.check(floatInch.toString(), "1412240x5895340 um");
    harness.check(intInch.toString(), "203200x304800 um");
    harness.check(floatMM.toString(), "55600x232100 um");
    harness.check(intMM.toString(), "210000x297000 um");
    
    harness.check(floatInch.toString(Size2DSyntax.INCH, null), "55.6x232.1");
    harness.check(intInch.toString(Size2DSyntax.INCH, null), "8.0x12.0");
    harness.check(floatMM.toString(Size2DSyntax.INCH, null), 
                  "2.1889763x9.137795");
    harness.check(intMM.toString(Size2DSyntax.INCH, null), 
                  "8.267716x11.692913");
    
    harness.check(floatInch.toString(Size2DSyntax.MM, "mm"), 
                  "1412.24x5895.34 mm");
    harness.check(intInch.toString(Size2DSyntax.MM, "mm"), "203.2x304.8 mm");
    harness.check(floatMM.toString(Size2DSyntax.MM, "mm"), "55.6x232.1 mm");
    harness.check(intMM.toString(Size2DSyntax.MM, "mm"), "210.0x297.0 mm");
  }

}
