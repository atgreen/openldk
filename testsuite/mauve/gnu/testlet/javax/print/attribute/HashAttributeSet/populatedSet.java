//Tags: JDK1.4
//Uses: SimpleAttribute AnotherSimpleAttribute

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

import javax.print.attribute.HashAttributeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests various methods for populated sets.
 */
public class populatedSet implements Testlet
{

  public void test(TestHarness harness)
  {
    
    SimpleAttribute att1 = new SimpleAttribute(1);
    
    HashAttributeSet testSet = new HashAttributeSet( att1 );
    
    // test basic functionality
    harness.check(testSet.containsValue(att1), true, "containsValue");
    harness.check(testSet.containsKey(att1.getCategory()), 
                  true, "containsKey");    
    harness.check(testSet.remove(att1), true, "remove");
    harness.check(testSet.isEmpty(), true, "isEmpty");
    harness.check(testSet.add(att1), true, "add");
    harness.check(testSet.add(att1), false, "re-add");
    
    // test category replacement
    SimpleAttribute att2 = new SimpleAttribute(3);
    SimpleAttribute[] array = 
      new SimpleAttribute[] { new SimpleAttribute(2), att2 };
    testSet = new HashAttributeSet( array );
    
    harness.check(testSet.size(), 1, "size");
    harness.check(testSet.hashCode(), 3, "hashcode");
    harness.check(testSet.containsValue(att2), true, "containsValue");
    
    AnotherSimpleAttribute att3 = new AnotherSimpleAttribute(4);
    
    harness.check(testSet.add(att3), true, "add");
    harness.check(testSet.size(), 2, "size");
    harness.check(testSet.hashCode(), 7, "hashcode");
    
    // build equal set for euqals test
    HashAttributeSet testSet2 = new HashAttributeSet();
    testSet2.add(att2);
    testSet2.add(att3);
    
    harness.check(testSet.equals(testSet2), true, "equals");    
    harness.check(testSet2.addAll(testSet), false, "addAll");
    testSet2.clear();
    harness.check(testSet2.isEmpty(), true, "isEmpty");
    
    // test hashcode
    testSet = new HashAttributeSet();
    testSet.add( new SimpleAttribute(1));
    testSet.add( new AnotherSimpleAttribute(2));
    
    testSet2 = new HashAttributeSet();
    testSet2.add( new SimpleAttribute(2));
    testSet2.add( new AnotherSimpleAttribute(1));
    
    harness.check(testSet.hashCode() == testSet2.hashCode(), "equal hashcode");
    harness.check(testSet.equals(testSet2), false, "no equality");
  
  }

}
