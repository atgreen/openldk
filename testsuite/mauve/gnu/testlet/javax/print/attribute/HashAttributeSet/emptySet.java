//Tags: JDK1.4
//Uses: SimpleAttribute

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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.print.attribute.HashAttributeSet;

/**
 * Test methods behaviour for an empty set.
 */
public class emptySet implements Testlet
{
  
  public void test(TestHarness harness)
  {
    HashAttributeSet testSet = new HashAttributeSet();
    HashAttributeSet testSet2 = new HashAttributeSet();
    
    harness.check(testSet.equals(testSet2), true, "equals");
    harness.check(testSet.hashCode(), 0, "hashcode");
    harness.check(testSet.toArray().length, 0, "toArray");
    
    harness.check(testSet.isEmpty(), true, "isEmpty 1"); 
    testSet.add(new SimpleAttribute(1));
    testSet.remove(new SimpleAttribute(1).getCategory());
    harness.check(testSet.isEmpty(), true, "isEmpty 2"); 
  }

}
