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

package gnu.testlet.javax.print.attribute.TextSyntax;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Locale;

import javax.print.attribute.TextSyntax;

/**
 * Tests the constructors of TextSyntax.
 */
public class constructors implements Testlet
{
  // Helper class to make abstract class usable.  
  public class TestTextSyntax extends TextSyntax 
  {
    public TestTextSyntax(String v, Locale l) {
      super(v,l);
    }    
  }  

  public void test(TestHarness harness)
  {
    harness.checkPoint("constructors");
    
    // null text value should trigger NPE
    try
      {
        new TestTextSyntax(null, Locale.GERMANY);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }    
  
    // null locale should use the default locale
    TestTextSyntax defaultLocale = new TestTextSyntax("Text", null);
    harness.check(defaultLocale.getLocale().equals(Locale.getDefault()));    
  }

}
