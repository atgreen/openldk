/* Copyright (C) 2001 ACUNIA

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.2

package gnu.testlet.java.util.AbstractSet;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

/**
*  Written by ACUNIA. <br>
*                        <br>
*  this file contains test for java.util.AbstractSet   <br>
*
*/
public class AcuniaAbstractSetTest extends AbstractSet implements Testlet
{
  protected TestHarness th;

  public void test (TestHarness harness)
    {
       th = harness;
       test_equals();
       test_hashCode();
     }


/**
* implemented. <br>
*
*/
  public void test_equals(){
    th.checkPoint("equals(java.lang.Object)boolean");
    AcuniaAbstractSetTest xas1 = new AcuniaAbstractSetTest();
    AcuniaAbstractSetTest xas2 = new AcuniaAbstractSetTest();
    th.check( xas1.equals(xas2) , "checking equality -- 1");
    th.check(!xas1.equals(null) , "checking equality -- 2");
    th.check(!xas1.equals(new Object()) , "checking equality -- 3");
    th.check( xas1.equals(xas1) , "checking equality -- 4");
    xas1.v.add(null);
    xas1.v.add("a");
    xas2.v.add("b");
    xas2.v.add(null);
    xas2.v.add("a");
    xas1.v.add("b");
    th.check( xas1.equals(xas2) , "checking equality -- 5");
    th.check( xas1.equals(xas1) , "checking equality -- 6");


  }
/**
* implemented. <br>
*
*/
  public void test_hashCode(){
    th.checkPoint("hashCode()int");
    AcuniaAbstractSetTest xas = new AcuniaAbstractSetTest();
    th.check(xas.hashCode() == 0 ,"checking hc-algorithm -- 1");
    xas.v.add(null);
    th.check(xas.hashCode() == 0 ,"checking hc-algorithm -- 2");
    xas.v.add("a");
    int hash = "a".hashCode();
    th.check(xas.hashCode() == hash ,"checking hc-algorithm -- 3");
    hash += "b".hashCode();
    xas.v.add("b");
    th.check(xas.hashCode() == hash ,"checking hc-algorithm -- 4");
    hash += "c".hashCode();
    xas.v.add("c");
    th.check(xas.hashCode() == hash ,"checking hc-algorithm -- 5");
    hash += "d".hashCode();
    xas.v.add("d");
    th.check(xas.hashCode() == hash ,"checking hc-algorithm -- 6");
  }

// The following methods aand field are needed to use this class as
// Set implementation.

    public Vector v = new Vector();

    public AcuniaAbstractSetTest(){
    	super();
    }

    public int size() {
    	return v.size();
    }

    public Iterator iterator() {
    	return v.iterator();
    }

}
