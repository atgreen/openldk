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
// Uses: Entry ESet EIterator

package gnu.testlet.java.util.AbstractMap;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

/**
*  Written by ACUNIA. <br>
*                        <br>
*  this file contains test for java.util.AbstractMap   <br>
*
*/
public class AcuniaAbstractMapTest extends AbstractMap implements Testlet
{
  protected TestHarness th;

  public void test (TestHarness harness)
  {
       th = harness;
       test_get();
       test_containsKey();
       test_containsValue();
       test_isEmpty();
       test_size();
       test_clear();
       test_put();
       test_putAll();
       test_remove();
       test_entrySet();
       test_keySet();
       test_values();
       test_equals();
       test_hashCode();
       test_toString();
  }

  protected AcuniaAbstractMapTest buildHT() {
   	AcuniaAbstractMapTest t = new AcuniaAbstractMapTest();
   	String s;
   	for (int i=0 ; i < 15 ; i++) {
   	 	s = "a"+i;
   	 	t.put(s,s+" value");
   	}
   	return t;
  }


/**
* implemented. <br>
*
*/
  public void test_get(){
    th.checkPoint("get(java.lang.Object)java.lang.Object");
    AcuniaAbstractMapTest ehm = buildHT();
    Object o;
    String s="a1";
    o = ehm.get(s);
    th.check( (s+" value").equals(o) , "checking return value");
    o = ehm.get(null);
    th.check( o == null );
    o = ehm.get(s+" value");
    th.check( o == null );
    ehm.put(null,s);
    o = ehm.get(null);
    th.check( s.equals(o));

  }

/**
* implemented. <br>
*
*/
  public void test_containsKey(){
    th.checkPoint("containsKey(java.lang.Object)boolean");
    AcuniaAbstractMapTest ehm = buildHT();
    th.check(!ehm.containsKey(null) , "null not there");
    ehm.put(null,"test");
    th.check(ehm.containsKey(null) , "null is in there");
    th.check(ehm.containsKey("a1") , "object is in there");
    th.check(!ehm.containsKey("a1 value") , "object is not in there -- 1");
    th.check(!ehm.containsKey(new Object()) , "object is not in there -- 2");

  }

/**
* implemented. <br>
*
*/
  public void test_containsValue(){
    th.checkPoint("containsValue(java.lang.Object)boolean");
    AcuniaAbstractMapTest ehm = buildHT();
    th.check(!ehm.containsValue(null) , "null not there");
    ehm.put(null,null);
    th.check(ehm.containsValue(null) , "null is in there");
    th.check(!ehm.containsValue("a1") , "object is not in there -- 1");
    th.check(ehm.containsValue("a1 value") , "object is in there -- 1");
    th.check(!ehm.containsValue(new Object()) , "object is not in there -- 2");

  }

/**
* implemented. <br>
*
*/
  public void test_isEmpty(){
    th.checkPoint("isEmpty()boolean");
    AcuniaAbstractMapTest ehm = new AcuniaAbstractMapTest();
    th.check(ehm.isEmpty() , "true");
    ehm = buildHT();
    th.check(!ehm.isEmpty() , "false");

  }

/**
*  not implemented. <br>
*  Abstract Method
*/
  public void test_size(){
    th.checkPoint("()");

  }

/**
* implemented. <br>
*
*/
  public void test_clear(){
    th.checkPoint("clear()void");
    AcuniaAbstractMapTest ehm = buildHT();
    ehm.clear();
    th.check(ehm.isEmpty() , "true");
  }

/**
* implemented. <br>
*
*/
  public void test_put(){
    th.checkPoint("put(java.lang.Object,java.lang.Object)java.lang.Object");
    AcuniaAbstractMapTest ehm = buildHT();
    ehm.set_edit(false);
    try {
    	ehm.put("a","b");
    	th.fail("should throw an UnsupportedOperationException");
    }
    catch (UnsupportedOperationException uoe) { th.check(true); }		
  }

/**
* implemented. <br>
*
*/
  public void test_putAll(){
    th.checkPoint("putAll(java.util.Map)void");
    Hashtable ht = new Hashtable();
    AcuniaAbstractMapTest ehm = new AcuniaAbstractMapTest();
    th.check( ehm.equals(ht) , "true -- both empty");
    ht.put("a","b");	ht.put("c","d");	ht.put("e","f");
    ehm.putAll(ht);
    th.check( ehm.equals(ht) , "true -- 1");
    ht.put("a1","f");
    ht.put("e","b");
    ehm.putAll(ht);
    th.check( ehm.equals(ht) , "true -- 2");
    ehm = buildHT();
    try {
      ehm.putAll(ht);
      th.check(true, "putAll: " + ht);
    }
    catch (NoSuchElementException nse) { th.check(false, "putAll: " + ht); }
    th.check(ehm.size() == 18 , "added three elements");
    th.check("f".equals(ehm.get("a1")) , "overwritten old value");
  }

/**
* implemented. <br>
*
*/
  public void test_remove(){
    th.checkPoint("remove(java.lang.Object)java.lang.Object");
    AcuniaAbstractMapTest ehm = buildHT();
    ehm.remove("a1");
    th.check(!ehm.containsKey("a1") , "key removed -- 1");
    th.check(!ehm.containsValue("a1 value") , "value removed -- 1");
    ehm.remove("a0");
    th.check(!ehm.containsKey("a0") , "key removed -- 2");
    th.check(!ehm.containsValue("a0 value") , "value removed -- 2");
    for (int i=2 ; i < 15 ; i++ ) {
	    ehm.remove("a"+i);
    }
    th.check(ehm.isEmpty());
  }

/**
*   not implemented. <br>
*   Abstract Method
*/
  public void test_entrySet(){
    th.checkPoint("()");

  }

/**
* implemented. <br>
* check only on methods not inherited from AbstractSet
*/
  public void test_keySet(){
    th.checkPoint("keySet()java.util.Set");
    AcuniaAbstractMapTest ehm = buildHT();
    Set s = ehm.keySet();
    th.check(s.size() == 15);
    ehm.put(null,"test");
    th.check(s.size() == 16);
    th.check(s.contains("a1"),"does contain a1");
    th.check(s.contains(null),"does contain null");
    th.check(!s.contains(new Object()),"does contain new Object");
    th.check(!s.contains("test"),"does contain test");
    th.check( s == ehm.keySet() , "same Set is returned");
    Iterator it = s.iterator();
    Vector v = ehm.getKeyV();
    int i;
    Object o;
    for (i=0 ; i < 16 ; i++) {
    	o = it.next();
    	th.check(v.indexOf(o) == 0, "order is not respected");
    	if (!v.remove(o)) th.debug("didn't find "+o);
     	
    }
    it = s.iterator();
    while (it.hasNext()) {
     	it.next();
     	it.remove();
    }
    th.check(s.isEmpty(), "everything is removed");
    s = ehm.keySet();
    th.check(s.isEmpty(), "new Set is also empty");
    ehm.put("a","B");
    th.check(!s.isEmpty(), "Set is updated by underlying actions");
  }

/**
* implemented. <br>
* check only on methods not inherited from AbstractCollection
*/
  public void test_values(){
    th.checkPoint("values()java.util.Collection");
    AcuniaAbstractMapTest ehm = buildHT();
    Collection s = ehm.values();
    th.check(s.size() == 15);
    ehm.put(null,"test");
    ehm.put("a10",null);
    th.check(s.size() == 16);
    th.check(s.contains("a1 value"),"does contain a1 value");
    th.check(s.contains(null),"does contain null");
    th.check(!s.contains(new Object()),"does contain new Object");
    th.check(s.contains("test"),"does contain test");
    th.check(!s.contains("a1"),"does not contain a1");
    th.check( s == ehm.values() , "same Set is returned");
    Iterator it = s.iterator();
    Vector v = ehm.getValuesV();
    int i;
    Object o;
    for (i=0 ; i < 16 ; i++) {
    	o = it.next();
    	th.check(v.indexOf(o) == 0, "order is not respected");
    	if (!v.remove(o)) th.debug("didn't find "+o);
     	
    }
    it = s.iterator();
    while (it.hasNext()) {
     	it.next();
     	it.remove();
    }
    th.check(s.isEmpty(), "everything is removed");
    s = ehm.values();
    th.check(s.isEmpty(), "new Set is also empty");
    ehm.put("a","B");
    th.check(!s.isEmpty(), "Set is updated by underlying actions");

  }

/**
* implemented. <br>
*
*/
  public void test_equals(){
    th.checkPoint("equals(java.lang.Object)boolean");
    Hashtable ht = new Hashtable();
    AcuniaAbstractMapTest ehm = new AcuniaAbstractMapTest();
    th.check( ehm.equals(ht) , "true -- both empty");
    ht.put("a","b");	ht.put("c","d");	ht.put("e","f");
    ehm.put("a","b");	ehm.put("c","d");	ehm.put("e","f");
    th.check( ehm.equals(ht) , "true -- same key && values");
    ht.put("a","f");
    th.check(! ehm.equals(ht) , "false -- same key && diff values");
    ht.put("e","b");
    th.check(! ehm.equals(ht) , "false --  key with diff values");
    th.check(! ehm.equals(ht.entrySet()) , "false --  no Map");
    th.check(! ehm.equals(new Object()) , "false -- Object is no Map");
    th.check(! ehm.equals(null) , "false -- Object is null");



  }

/**
* implemented. <br>
*
*/
  public void test_hashCode(){
    th.checkPoint("hashCode()int");
    AcuniaAbstractMapTest ehm = new AcuniaAbstractMapTest();
    th.check( ehm.hashCode() == 0 , "hashCode of Empty Map is 0, got "+ehm.hashCode());
    int hash = 0;
    Iterator s = ehm.entrySet().iterator();
    while (s.hasNext()) { hash += s.next().hashCode(); }
    th.check( ehm.hashCode() , hash , "hashCode of Empty Map -- checking Algorithm");

  }

/**
* implemented. <br>
*
*/
  public void test_toString(){
    th.checkPoint("toString()java.lang.String");
    AcuniaAbstractMapTest ehm = new AcuniaAbstractMapTest();
    th.check("{}".equals(ehm.toString()) , "checking empty Map");
    ehm.put("a","b");	
    th.debug(ehm.toString());
    th.check("{a=b}".equals(ehm.toString()) , "checking Map with one element");
    ehm.put("c","d");	ehm.put("e","f");
    th.debug(ehm.toString());
    th.check("{a=b, c=d, e=f}".equals(ehm.toString()) , "checking Map with three elements");
  }

  public String toString() {
	  return super.toString();
  }

// The following field and methods are needed to use this class as an
// implementation test for AbstractMap
//
	Vector keys = new Vector();
	Vector values = new Vector();
	private boolean edit = true;
	
	boolean deleteInAM(Object e) {
	 	if  (!keys.contains(e)) return false;
	 	values.remove(keys.indexOf(e));
	 	return keys.remove(e);
	}
	
	public Vector getKeyV() {
		return (Vector)keys.clone();
	}
	public Vector getValuesV() {
		return (Vector)values.clone();
	}
	
	public AcuniaAbstractMapTest(){
		super();
	}
	
	public Set entrySet() {
		return  new ESet(this);
	}

	public Object put(Object key, Object value) {
		if (edit) {
			if (keys.contains(key)) {
				return values.set(keys.indexOf(key),value);
			}
			values.add(value);
			keys.add(key);
			return null;
		}
		return super.put(key,value);
	}
	
	public void set_edit(boolean b) {
		edit = b;
	}
}
