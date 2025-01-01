//Tags: JDK1.4
//Uses: WrongEnumSyntax CorrectEnumSyntax

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
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */


package gnu.testlet.javax.print.attribute.EnumSyntax;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests that two instances behave correctly for equal, hashcode, 
 * identity test and toString methods.
 */
public class equals implements Testlet
{

  public void test(TestHarness harness)
  {
    // Also its not a correctly subclass of EnumSyntax
    // it is used here to test some fallback mechanisms.
    WrongEnumSyntax test = WrongEnumSyntax.TEST;
    WrongEnumSyntax test2 = WrongEnumSyntax.TEST;

    harness.check(test.hashCode() == 100, "hashcode()");
    harness.check(test.equals(test2), "equals()");
    harness.check(test == test2, "identity");
    harness.check(test.toString(), "100", "toString");
    
    CorrectEnumSyntax test3 = CorrectEnumSyntax.TEST3;
    CorrectEnumSyntax test4 = CorrectEnumSyntax.TEST3;

    harness.check(test3.hashCode() == 5, "hashcode()");
    harness.check(test3.equals(test4), "equals()");
    harness.check(test3 == test4, "identity");
    harness.check(test3.toString(), "test3", "toString");
  }
  
}
