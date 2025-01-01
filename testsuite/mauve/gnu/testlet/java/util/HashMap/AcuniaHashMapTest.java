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

package gnu.testlet.java.util.HashMap;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;
import java.lang.reflect.*;
/**
*  Written by ACUNIA. <br>
*                        <br>
*  this file contains test for java.util.HashMap   <br>
*
*/
public class AcuniaHashMapTest implements Testlet
{
  protected TestHarness th;

  public void test (TestHarness harness)   {
       th = harness;
       test_HashMap();
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
       test_clone();
       test_behaviour();
  }

  protected HashMap buildHM() {
  	HashMap hm = new HashMap();
  	String s;
  	for (int i=0 ; i < 15 ; i++) {
  		s = "a"+i;
  		hm.put(s , s+" value");
  	}
  	hm.put(null,null);
  	return hm;
  }	

/**
* implemented. <br>
*
*/
  public void test_HashMap(){
    Field lf = null;
    try {
   	lf = HashMap.class.getDeclaredField("loadFactor");
//   	th.debug("DEBUG -- found loadFactor");
   	lf.setAccessible(true);
    }
    catch(Exception e){}
    HashMap hm;
   th.checkPoint("HashMap()");
    hm = new HashMap();
    try {
        th.check(lf.getFloat(hm), 0.75f);
        }
    catch (Exception e) { th.fail("no exception wanted !!!, got "+e); }
   th.checkPoint("HashMap(java.util.Map)");
    HashMap hm1 = buildHM();
    hm = new HashMap(hm1);
    try {
        th.check(lf.getFloat(hm), 0.75f);
        }
    catch (Exception e) { th.fail("no exception wanted !!!, got "+e); }
    th.check(hm.size() == 16 , "all elements are put, got "+hm.size());
    th.check(hm.get(null) == null , "test key and value pairs -- 1");
    th.check("a1 value".equals(hm.get("a1")) , "test key and value pairs -- 2");
    th.check("a10 value".equals(hm.get("a10")) , "test key and value pairs -- 3");
    th.check("a0 value".equals(hm.get("a0")) , "test key and value pairs -- 4");
    hm = new HashMap(new Hashtable());
    th.check(hm.size() == 0 , "no elements are put, got "+hm.size());
    try {
   	new HashMap(null);
   	th.fail("should throw a NullPointerException");
    }
    catch(NullPointerException ne) {th.check(true);}

   th.checkPoint("HashMap(int)");
    hm = new HashMap(1);
    try {
        th.check(lf.getFloat(hm), 0.75f);
        }
    catch (Exception e) { th.fail("no exception wanted !!!, got "+e); }
    try { new HashMap(-1);
   	 th.fail("should throw an IllegalArgumentException");
        }
    catch(IllegalArgumentException iae) { th.check(true); }

   th.checkPoint("HashMap(int,int)");
    hm = new HashMap(10,0.5f);
    try {
        th.check(lf.getFloat(hm), 0.5f);
        }
    catch (Exception e) { th.fail("no exception wanted !!!, got "+e); }
    hm = new HashMap(10,1.5f);
    try {
        th.check(lf.getFloat(hm), 1.5f);
        }
    catch (Exception e) { th.fail("no exception wanted !!!, got "+e); }
    try {new HashMap(-1,0.1f);
       	 th.fail("should throw an IllegalArgumentException -- 1");
        }
    catch(IllegalArgumentException iae) { th.check(true); }
    try { new HashMap(1,-0.1f);
   	 th.fail("should throw an IllegalArgumentException -- 2");
        }
    catch(IllegalArgumentException iae) { th.check(true); }
    try { new HashMap(1,0.0f);
    	 th.fail("should throw an IllegalArgumentException -- 2");
        }
    catch(IllegalArgumentException iae) { th.check(true); }

  }

/**
* implemented. <br>
*
*/
  public void test_get(){
    th.checkPoint("get(java.lang.Object)java.lang.Object");
    HashMap hm = buildHM();
    th.check(hm.get(null) == null , "checking get -- 1");
    th.check(hm.get(this) == null , "checking get -- 2");
    hm.put("a" ,this);
    th.check("a1 value".equals(hm.get("a1")), "checking get -- 3");
    th.check("a11 value".equals(hm.get("a11")), "checking get -- 4");
    th.check( hm.get(new Integer(97)) == null , "checking get -- 5");


  }

/**
* implemented. <br>
*
*/
  public void test_containsKey(){
    th.checkPoint("containsKey(java.lang.Object)boolean");
    HashMap hm = new HashMap();
    hm.clear();
    th.check(! hm.containsKey(null) ,"Map is empty");
    hm.put("a" ,this);
    th.check(! hm.containsKey(null) ,"Map does not containsthe key -- 1");
    th.check( hm.containsKey("a") ,"Map does contain the key -- 2");
    hm = buildHM();
    th.check( hm.containsKey(null) ,"Map does contain the key -- 3");
    th.check(! hm.containsKey(this) ,"Map does not contain the key -- 4");

  }

/**
* implemented. <br>
*
*/
  public void test_containsValue(){
    th.checkPoint("containsValue(java.lang.Object)boolean");
    HashMap hm = new HashMap();
    hm.clear();
    th.check(! hm.containsValue(null) ,"Map is empty");
    hm.put("a" ,this);
    th.check(! hm.containsValue(null) ,"Map does not containsthe value -- 1");
    th.check(! hm.containsValue("a") ,"Map does  not contain the value -- 2");
    th.check( hm.containsValue(this) ,"Map does contain the value -- 3");
    hm = buildHM();
    th.check( hm.containsValue(null) ,"Map does contain the value -- 4");
    th.check(! hm.containsValue(this) ,"Map does not contain the value -- 5");
    th.check(! hm.containsValue("a1value") ,"Map does  not contain the value -- 6");

  }

/**
* implemented. <br>
*
*/
  public void test_isEmpty(){
    th.checkPoint("isEmpty()boolean");
    HashMap hm = new HashMap();
    th.check( hm.isEmpty() ,"Map is empty");
    hm.put("a" ,this);
    th.check(! hm.isEmpty() ,"Map is not empty");

  }

/**
* implemented. <br>
*
*/
  public void test_size(){
    th.checkPoint("size()int");
    HashMap hm = new HashMap();
    th.check(hm.size() == 0 ,"Map is empty");
    hm.put("a" ,this);
    th.check(hm.size() == 1 ,"Map has 1 element");
    hm = buildHM();
    th.check(hm.size() == 16 ,"Map has 16 elements");

  }

/**
* implemented. <br>
*
*/
  public void test_clear(){
    th.checkPoint("clear()void");
    HashMap hm = buildHM();
    hm.clear();
    th.check(hm.size() == 0 ,"Map is cleared -- 1");
    th.check(hm.isEmpty() ,"Map is cleared -- 2");
	
  }

/**
* implemented. <br>
* is tested also in the other parts ...
*/
  public void test_put(){
    th.checkPoint("put(java.lang.Object,java.lang.Object)java.lang.Object");
    HashMap hm  = new HashMap();
    th.check( hm.put(null , this ) == null , "check on return value -- 1");
    th.check( hm.get(null) == this , "check on value -- 1");
    th.check( hm.put(null , "a" ) == this , "check on return value -- 2");
    th.check( "a".equals(hm.get(null)) , "check on value -- 2");
    th.check( "a".equals(hm.put(null , "a" )), "check on return value -- 3");
    th.check( "a".equals(hm.get(null)) , "check on value -- 3");
    th.check( hm.size() == 1 , "only one key added");
    th.check( hm.put("a" , null ) == null , "check on return value -- 4");
    th.check( hm.get("a") == null , "check on value -- 4");
    th.check( hm.put("a" , this ) == null , "check on return value -- 5");
    th.check( hm.get("a") == this , "check on value -- 5");
    th.check( hm.size() == 2 , "two keys added");

  }

/**
* implemented. <br>
*
*/
  public void test_putAll(){
    th.checkPoint("putAll(java.util.Map)void");
    HashMap hm  = new HashMap();
    hm.putAll(new Hashtable());
    th.check(hm.isEmpty() , "nothing addad");
    hm.putAll(buildHM());
    th.check(hm.size() == 16 , "checking if all enough elements are added -- 1");
    th.check(hm.equals(buildHM()) , "check on all elements -- 1");
    hm.put(null ,this);
    hm.putAll(buildHM());
    th.check(hm.size() == 16 , "checking if all enough elements are added -- 2");
    th.check(hm.equals(buildHM()) , "check on all elements -- 2");
    try {
    	hm.putAll(null);
    	th.fail("should throw a NullPointerException");
    }
    catch(NullPointerException npe) { th.check(true); }	
  }

/**
* implemented. <br>
*
*/
  public void test_remove(){
    th.checkPoint("remove(java.lang.Object)java.lang.Object");
    HashMap hm  = buildHM();
    th.check(hm.remove(null) == null , "checking return value -- 1");
    th.check(hm.remove(null) == null , "checking return value -- 2");
    th.check(!hm.containsKey(null) , "checking removed key -- 1");
    th.check(!hm.containsValue(null) , "checking removed value -- 1");
    for (int i = 0 ; i < 15 ; i++) {
    	th.check( ("a"+i+" value").equals(hm.remove("a"+i)), " removing a"+i);
    }
    th.check(hm.isEmpty() , "checking if al is gone");
  }

/**
* implemented. <br>
* uses AbstractSet --> check only the overwritten methods ... !
* iterator and size
* fail-fast iterator !
* add not supported !
* check the Map.Entry Objects ...
*/
  public void test_entrySet(){
    th.checkPoint("entrySet()java.util.Set");
    HashMap hm  = buildHM();
    Set s = hm.entrySet();
    Iterator it= s.iterator();
    Map.Entry me=null;
    it.next();
    try {
    	s.add("ADDING");
    	th.fail("add should throw an UnsupportedOperationException");
    }
    catch (UnsupportedOperationException uoe) { th.check(true); }
    th.check( s.size() == 16 );
    hm.remove("a12");
    th.check( s.size() == 15 );
    try {
    	th.check(it.hasNext());
	th.check(true);
    }
    catch(ConcurrentModificationException cme) {
	th.fail("it.hasNext should not throw ConcurrentModificationException");
    }
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- 1");
    }
    catch(ConcurrentModificationException cme){ th.check(true); }
    try {
    	it.remove();
    	th.fail("should throw a ConcurrentModificationException -- 2");
    }
    catch(ConcurrentModificationException cme){ th.check(true); }
//    th.debug(hm.debug());
    it= s.iterator();
    try {
    	me = (Map.Entry)it.next();
//    	Thread.sleep(600L);
    	if (me.getKey()==null) me = (Map.Entry)it.next();
    	th.check( me.hashCode() , (me.getValue().hashCode() ^ me.getKey().hashCode()),"verifying hashCode");
    	th.check(! me.equals(it.next()));
    	
    	}
    catch(Exception e) { th.fail("got unwanted exception ,got "+e);
    	th.debug("got ME key = "+me+" and value = "+me.getKey());}

    try {
//     th.debug("got ME key = "+me.getKey()+" and value = "+me.getValue());
    	me.setValue("alpha");
    	th.check(hm.get(me.getKey()), "alpha", "setValue through iterator of entrySet");
    	}
    catch(UnsupportedOperationException uoe) { th.fail("setValue should be supported");}
    it= s.iterator();
    Vector v = new Vector();
    Object ob;
    v.addAll(s);
    while (it.hasNext()) {
    	ob = it.next();
    	it.remove();
     	if (!v.remove(ob))
        th.debug("Object "+ob+" not in the Vector");
     }
     th.check( v.isEmpty() , "all elements gone from the vector");
//     for (int k=0 ; k < v.size() ; k++ ) { th.debug("got "+v.get(k)+" as element "+k); }
     th.check( hm.isEmpty() , "all elements removed from the HashMap");
    it= s.iterator();
    hm.put(null,"sdf");
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- 3");
    }
    catch(ConcurrentModificationException cme){ th.check(true); }
    it= s.iterator();
    hm.clear();
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- 4");
    }
    catch(ConcurrentModificationException cme){ th.check(true); }

  }

/**
* implemented. <br>
* uses AbstractSet --> check only the overwritten methods ... !
* iterator and size
* fail-fast iterator !
* add not supported !
*/
  public void test_keySet(){
    th.checkPoint("keySet()java.util.Set");
    HashMap hm = buildHM();
    th.check( hm.size() == 16 , "checking map size(), got "+hm.size());
    Set s=null;
    Object [] o;
    Iterator it;
    try {
        s = hm.keySet();
        th.check( s != null ,"s != null");
        th.check(s.size() == 16 ,"checking size keyset, got "+s.size());
        o = s.toArray();
        th.check( o != null ,"o != null");
        th.check( o.length == 16 ,"checking length, got "+o.length);
//        for (int i = 0 ; i < o.length ; i++ ){ th.debug("element "+i+" is "+o[i]); }
	it = s.iterator();
	Vector v = new Vector();
	Object ob;
	v.addAll(s);
	while ( it.hasNext() ) {
        	ob = it.next();
        	it.remove();
        	if (!v.remove(ob))
        	th.debug("Object "+ob+" not in the Vector");
        }
        th.check( v.isEmpty() , "all elements gone from the vector");
        th.check( hm.isEmpty() , "all elements removed from the HashMap");
    }
    catch (Exception e) { th.fail("got bad Exception -- got "+e); }
    try {
    	s.add("ADDING");
    	th.fail("add should throw an UnsupportedOperationException");
    }
    catch (UnsupportedOperationException uoe) { th.check(true); }

  }

/**
* implemented. <br>
* uses AbstractCollection --> check only the overwritten methods ... !
* iterator and size
* fail-fast iterator !
* add not supported !
*/
  public void test_values(){
    th.checkPoint("values()java.util.Collection");
    HashMap hm = buildHM();
    th.check( hm.size() == 16 , "checking map size(), got "+hm.size());
    Collection s=null;
    Object [] o;
    Iterator it;
    try {
        s = hm.values();
        th.check( s != null ,"s != null");
        th.check(s.size() == 16 ,"checking size keyset, got "+s.size());
        o = s.toArray();
        th.check( o != null ,"o != null");
        th.check( o.length == 16 ,"checking length, got "+o.length);
//        for (int i = 0 ; i < o.length ; i++ ){ th.debug("element "+i+" is "+o[i]); }
	it = s.iterator();
	Vector v = new Vector();
	Object ob;
	v.addAll(s);
	while ( it.hasNext() ) {
        	ob = it.next();
        	it.remove();
        	if (!v.remove(ob))
        	th.debug("Object "+ob+" not in the Vector");
        }
        th.check( v.isEmpty() , "all elements gone from the vector");
        th.check( hm.isEmpty() , "all elements removed from the HashMap");
    }
    catch (Exception e) { th.fail("got bad Exception -- got "+e); }
    try {
    	s.add("ADDING");
    	th.fail("add should throw an UnsupportedOperationException");
    }
    catch (UnsupportedOperationException uoe) { th.check(true); }


  }

/**
* implemented. <br>
*
*/
  public void test_clone(){
    th.checkPoint("clone()java.lang.Object");
    HashMap hm = buildHM();
    Object o = hm.clone();
    th.check( o != hm , "clone is not the same object");
    th.check( hm.equals(o) , "clone is equal to Map");
    hm.put("a","b");
    th.check(! hm.equals(o) , "clone doesn't change if Map changes");

  }
/**
* the goal of this test is to see how the hashtable behaves if we do a lot put's and removes. <br>
* we perform this test for different loadFactors and a low initialsize <br>
* we try to make it difficult for the table by using objects with same hashcode
*/
  private final String st ="a";
  private final Byte b =new Byte((byte)97);
  private final Short sh=new Short((short)97);
  private final Integer i = new Integer(97);
  private final Long l = new Long(97L);
  private int sqnce = 1;

  public void test_behaviour(){
    th.checkPoint("behaviour testing");
//    do_behaviourtest(0.2f);
    do_behaviourtest(0.70f);
    do_behaviourtest(0.75f);
    do_behaviourtest(0.95f);
    do_behaviourtest(1.0f);

    }
  protected void sleep(int time){
  	try { Thread.sleep(time); }
  	catch (Exception e) {}	
  }

  protected void check_presence(HashMap h){
    th.check( h.get(st) != null, "checking presence st -- sequence "+sqnce);
    th.check( h.get(sh) != null, "checking presence sh -- sequence "+sqnce);
    th.check( h.get(i) != null, "checking presence i -- sequence "+sqnce);
    th.check( h.get(b) != null, "checking presence b -- sequence "+sqnce);
    th.check( h.get(l) != null, "checking presence l -- sequence "+sqnce);
    sqnce++;
  }

  protected void do_behaviourtest(float loadFactor) {

    th.checkPoint("behaviour testing with loadFactor "+loadFactor);
    HashMap h = new HashMap(11 , loadFactor);
    int j=0;
    Float f;
    h.put(st,"a"); h.put(b,"byte"); h.put(sh,"short"); h.put(i,"int"); h.put(l,"long");
    check_presence(h);
    sqnce = 1;
    for ( ; j < 100 ; j++ )
    {   f = new Float((float)j);
        h.put(f,f);
       // sleep(5);
    }
    th.check(h.size() == 105,"size checking -- 1 got: "+h.size());
    check_presence(h);
//    sleep(500);
    for ( ; j < 200 ; j++ )
    {   f = new Float((float)j);
        h.put(f,f);
      //  sleep(10);
    }
    th.check(h.size() == 205,"size checking -- 2 got: "+h.size());
    check_presence(h);
//    sleep(50);

    for ( ; j < 300 ; j++ )
    {   f = new Float((float)j);
        h.put(f,f);
      //  sleep(10);
    }
    th.check(h.size() == 305,"size checking -- 3 got: "+h.size());
    check_presence(h);
//    sleep(50);
// replacing values -- checking if we get a non-zero value
    th.check("a".equals(h.put(st,"na")), "replacing values -- 1 - st");
    th.check("byte".equals(h.put(b,"nbyte")), "replacing values -- 2 - b");
    th.check("short".equals(h.put(sh,"nshort")), "replacing values -- 3 -sh");
    th.check("int".equals(h.put(i,"nint"))  , "replacing values -- 4 -i");
    th.check("long".equals(h.put(l,"nlong")), "replacing values -- 5 -l");


    for ( ; j > 199 ; j-- )
    {   f = new Float((float)j);
        h.remove(f);
      //  sleep(10);
    }
//    sleep(150);
    th.check(h.size() == 205,"size checking -- 4 got: "+h.size());
    check_presence(h);
    for ( ; j > 99 ; j-- )
    {   f = new Float((float)j);
        h.remove(f);
      //  sleep(5);
    }
    th.check(h.size() == 105,"size checking -- 5 got: "+h.size());
    check_presence(h);
   // sleep(1500);
    for ( ; j > -1 ; j-- )
    {   f = new Float((float)j);
        h.remove(f);
     //   sleep(5);
    }
    th.check(h.size() == 5  ,"size checking -- 6 got: "+h.size());

    th.debug(h.toString());
    check_presence(h);
   // sleep(500);

    }

}
