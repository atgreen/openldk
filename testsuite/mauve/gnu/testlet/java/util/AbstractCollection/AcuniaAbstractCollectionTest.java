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

package gnu.testlet.java.util.AbstractCollection;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

/**
*  Written by ACUNIA. <br>
*                        <br>
*  this file contains tests for java.util.AbstractCollection   <br>
*
*/
public class AcuniaAbstractCollectionTest extends AbstractCollection
                                          implements Testlet
{
  protected TestHarness th;

  public void test (TestHarness harness)
    {
       th = harness;
       test_addAll();
       test_clear();
       test_remove();
       test_removeAll();
       test_retainAll();
       test_contains();
       test_containsAll();
       test_isEmpty();
       test_size();
       test_iterator();
       test_toArray();
       test_toString();

     }

/**
* implemented. <br>
*
*/
  public void test_addAll(){
    th.checkPoint("addAll(java.util.Collection)boolean");
    Vector v = new Vector();
    v.add("a"); 	v.add("b");
    v.add("c");         v.add("d");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    th.check( ac.addAll(v) , "should return true, v is modified");
    th.check( ac.v.equals(v) , "check everything is added");
    ac.setRA(false);
    th.check(! ac.addAll(v) , "should return false, v is  not modified");
    th.check( ac.v.equals(v) , "check everything is added");
    try {
    	ac.addAll(null);
    	th.fail("should throw a NullPointerException");
    	}
    catch (NullPointerException ne) { th.check(true);}
  }

/**
* implemented. <br>
*
*/
  public void test_clear(){
    th.checkPoint("clear()void");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    ac.v.add("a"); 	ac.v.add("b");
    ac.v.add("c");      ac.v.add("d");
    ac.clear();
    th.check(ac.size()==0 , "all elements are removed -- 1");
    ac.clear();
    th.check(ac.size()==0 , "all elements are removed -- 2");
  }

/**
* implemented. <br>
*
*/
  public void test_remove(){
    th.checkPoint("remove(java.lang.Object)boolean");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    ac.v.add("a"); 	ac.v.add(null);
    ac.v.add("c");      ac.v.add("a");
    th.check(ac.remove("a"), "returns true if removed -- 1");
    th.check(ac.size()==3 , "one element was removed -- 1");
    th.check(!"a".equals(ac.v.get(0)) , "check if correct element was removed");
    th.check(ac.remove("a"), "returns true if removed -- 2");
    th.check(ac.size()==2 , "one element was removed -- 2");
    th.check(!ac.remove("a"), "returns false if not removed -- 3");
    th.check(ac.size()==2 , "no elements were removed -- 3");
    th.check(ac.remove(null), "returns true if removed -- 4");
    th.check(ac.size()==1 , "one element was removed -- 4");
    th.check(!ac.remove(null), "returns false if not removed -- 5");
    th.check(ac.size()==1 , "no elements were removed -- 5");
    th.check("c".equals(ac.v.get(0)) , "\"c\" is left");
  }

/**
* implemented. <br>
*
*/
  public void test_removeAll(){
    th.checkPoint("removeAll(java.util.Collection)boolean");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    ac.v.add("a"); 	ac.v.add(null);
    ac.v.add("c");      ac.v.add("a");
    try {
    	ac.removeAll(null);
    	th.fail("should throw a NullPointerException");
    	}
    catch (NullPointerException ne) { th.check(true);}
    Vector v = new Vector();
    v.add("a"); v.add(null); v.add("de"); v.add("fdf");
    th.check( ac.removeAll(v) , "should return true");
    th.check( ac.size() == 1 , "duplicate elements are removed");
    th.check("c".equals(ac.v.get(0)) , "check if correct elements were removed");
    th.check(! ac.removeAll(v) , "should return false");
    th.check( ac.size() == 1 , "no elements were removed");

  }

/**
* implemented. <br>
*
*/
  public void test_retainAll(){
    th.checkPoint("retainAll(java.util.Collection)boolean");
    AcuniaAbstractCollectionTest ac = new  AcuniaAbstractCollectionTest();
    ac.v.add("a"); 	ac.v.add(null);
    ac.v.add("c");      ac.v.add("a");
    try {
    	ac.retainAll(null);
    	th.fail("should throw a NullPointerException");
    	}
    catch (NullPointerException ne) { th.check(true);}
    Vector v = new Vector();
    v.add("a"); v.add(null); v.add("de"); v.add("fdf");
    th.check( ac.retainAll(v) , "should return true");
    th.check( ac.size() == 3 , "duplicate elements are retained");
    th.check(! ac.retainAll(v) , "should return false");
    th.check( ac.size() == 3 , "all elements were retained");
    th.check( ac.v.contains(null) && ac.v.contains("a"));
  }

/**
* implemented. <br>
*
*/
  public void test_contains(){
    th.checkPoint("contains(java.lang.Object)boolean");
    AcuniaAbstractCollectionTest ac = new  AcuniaAbstractCollectionTest();
    ac.v.add("a"); 	ac.v.add(null);
    ac.v.add("c");      ac.v.add("a");
    th.check(ac.contains("a") , "true -- 1");
    th.check(ac.contains(null) , "true -- 2");
    th.check(ac.contains("c") , "true -- 3");
    th.check(!ac.contains("ab") , "false -- 4");
    th.check(!ac.contains("b") , "false -- 5");
    ac.remove(null);
    th.check(!ac.contains(null) , "false -- 4");
	
  }

/**
* implemented. <br>
*
*/
  public void test_containsAll(){
    th.checkPoint("containsAll(java.util.Collection)boolean");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    ac.v.add("a"); 	ac.v.add(null);
    ac.v.add("c");      ac.v.add("a");
    try {
    	ac.containsAll(null);
    	th.fail("should throw a NullPointerException");
    	}
    catch (NullPointerException ne) { th.check(true);}
    Vector v = new Vector();
    th.check( ac.containsAll(v) , "should return true -- 1");
    v.add("a"); v.add(null); v.add("a"); v.add(null); v.add("a");
    th.check( ac.containsAll(v) , "should return true -- 2");
    v.add("c");
    th.check( ac.containsAll(v) , "should return true -- 3");
    v.add("c+");
    th.check(! ac.containsAll(v) , "should return false -- 4");
    v.clear();
    ac.clear();
    th.check( ac.containsAll(v) , "should return true -- 5");

  }

/**
* implemented. <br>
*
*/
  public void test_isEmpty(){
    th.checkPoint("isEmpty()boolean");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    th.check(ac.isEmpty() , "should return true -- 1");
    th.check(ac.isEmpty() , "should return true -- 2");
    ac.v.add(null);
    th.check(!ac.isEmpty() , "should return false -- 3");
    ac.clear();
    th.check(ac.isEmpty() , "should return true -- 4");

  }

/**
*   not implemented. <br>
*   Abstract Method
*/
  public void test_size(){
    th.checkPoint("()");
  }
/**
*   not implemented. <br>
*   Abstract Method
*/
  public void test_iterator(){
    th.checkPoint("()");
  }

/**
* implemented. <br>
*
*/
  public void test_toArray(){
   th.checkPoint("toArray()[java.lang.Object");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    Object [] oa = ac.toArray();
    th.check( oa != null , "returning null is not allowed");
    if (oa != null) th.check(oa.length == 0 , "empty array");
    ac.v.add("a"); 	ac.v.add(null);
    ac.v.add("c");      ac.v.add("a");
    oa = ac.toArray();
    th.check(oa[0].equals("a") && oa[1] == null && oa[2].equals("c") && oa[3].equals("a"), "checking elements");

   th.checkPoint("toArray([java.lang.Object)[java.lang.Object");
    try {
    	ac.toArray(null);
    	th.fail("should throw a NullPointerException");
    	}
    catch (NullPointerException ne) { th.check(true);}
    String [] sa = new String[5];
    for (int i = 0 ; i < 5 ; i++ ){ sa[i] ="ok"; }
    oa = ac.toArray(sa);
    th.check(oa[0].equals("a") && oa[1] == null && oa[2].equals("c") && oa[3].equals("a"), "checking elements");
    th.check(oa == sa , "array large enough --> fill + return it");
    th.check(sa[4] == null ,  "element at 'size' is set to null");

    sa = new String[3];
    for (int i = 0 ; i < 3 ; i++ ){ sa[i] ="ok"; }
    oa = ac.toArray(sa);
    th.check(oa[0].equals("a") && oa[1] == null && oa[2].equals("c") && oa[3].equals("a"), "checking elements");
    th.check ( oa instanceof String[] , "checking  class type of returnvalue");
    sa = new String[4];
    Class asc = sa.getClass();
    for (int i = 0 ; i < 4 ; i++ ){ sa[i] ="ok"; }
    oa = ac.toArray(sa);
    th.check(oa[0].equals("a") && oa[1] == null && oa[2].equals("c") && oa[3].equals("a"), "checking elements");
    th.check ( oa instanceof String[] , "checking  class type of returnvalue");
    th.check(oa == sa , "array large enough --> fill + return it");
  }
/**
* implemented. <br>
*
*/
  public void test_toString(){
    th.checkPoint("toString()java.lang.String");
    AcuniaAbstractCollectionTest ac = new AcuniaAbstractCollectionTest();
    ac.v.add("smartmove"); 	ac.v.add(null);
    ac.v.add("rules");      	ac.v.add("cars");
    String s = ac.toString();
    th.check( s.indexOf("smartmove") != -1 , "checking representations");
    th.check( s.indexOf("rules") != -1 , "checking representations");
    th.check( s.indexOf("cars") != -1 , "checking representations");
    th.check( s.indexOf("null") != -1 , "checking representations");
    th.debug(s);
  }

// The following fields and methods are needed to use this class as a
// AbstractCollection implementation.

	public Vector v;
	private boolean retadd=true;
	

	public AcuniaAbstractCollectionTest(){
		super();
		v=new Vector();
	}
	
	public int size() {
		return v.size();
	}
		
	
	public boolean add(Object o) {
		if (retadd)v.add(o);
		return retadd;
	}

	
	public Iterator iterator() {
		return v.iterator();
	}
	
	public void setRA(boolean b) {
		retadd=b;
	}
}
