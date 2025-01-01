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
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */


package gnu.testlet.javax.print.attribute.HashAttributeSet;

import javax.print.attribute.Attribute;
import javax.print.attribute.AttributeSet;
import javax.print.attribute.HashAttributeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests behaviour for various methods if parameters are null.
 */
public class nullTests implements Testlet
{

  public void test(TestHarness harness)
  {
    harness.checkPoint("constructor tests");
    try
      {
        new HashAttributeSet((Attribute) null);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }
    
    try
      {
        new HashAttributeSet((AttributeSet) null);
        harness.check(true);
      }
    catch (NullPointerException e)
      {
        harness.check(false);
      }
    
    try
      {
        new HashAttributeSet((Attribute[]) null);
        harness.check(true);
      }
    catch (NullPointerException e)
      {
        harness.check(false);
      }

    try
      {
        new HashAttributeSet(new Attribute[] { null });
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    harness.checkPoint("method tests");
    HashAttributeSet testSet = new HashAttributeSet();

    try
      {
        testSet.get(null);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    try
      {
        testSet.add(null);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    try
      {
        testSet.addAll(null);
        harness.check(false);
      }
    catch (NullPointerException e)
      {
        harness.check(true);
      }

    harness.check(testSet.remove((Attribute) null), false);
    harness.check(testSet.remove((Class) null), false);
    harness.check(testSet.containsKey(null), false);
    harness.check(testSet.containsValue(null), false);    
  }

}
