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

package gnu.testlet.java.util.Hashtable;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.*;

/**
* This file contains testcode for java.util.Hashtable <br>
*<br>
* WRITTEN BY ACUNIA <br>
*/
public class AcuniaHashtableTest implements Testlet
{
  protected TestHarness th;

  public void test (TestHarness harness)
    {
       th = harness;
       test_Hashtable();
       test_elements();
       test_get ();
       test_keys ();
       test_contains();
       test_containsKey();
       test_containsValue();
       test_isEmpty();
       test_size();
       test_put();
       test_putAll ();
       test_remove();
       test_entrySet();
       test_keySet();
       test_values();
       test_clone();
       test_equals();
       test_hashCode();
       test_toString();
       test_rehash();
       test_behaviour();
     }
  public Hashtable buildknownHt() {

    	Hashtable ht = new  Hashtable(19);
    	Float f;
    	for (int i =0; i < 11; i++)
    	{ f = new Float((float)i);
    	  ht.put( f , f );
        }
        return ht;

  }
/**
*  implemented.	<br>
*  testing this is not easy since we cannot get the values of <br>
*  the properties we pass to the hashtable --> code looked OK <br>
*  we just make the objects and do some basic testing.
*/
  public void test_Hashtable(){
    th.checkPoint("Hashtable()");
    Hashtable h = new Hashtable();
    h = new Hashtable(233, 0.5f);
    try {
    	h = new Hashtable(0);
    	th.check(true,"test 1");
	h = new Hashtable(25);
    	th.check(true,"test 2");
        }
    catch (Exception e) {th.fail("shouldn't throw an exception -- "+e);}

    try {
    	h = new Hashtable(-233);
	th.fail("should throw an IllegalArgumentException");
	}
    catch (IllegalArgumentException ie) { th.check(true,"test 3");}

    // what happens if loadfactor is 0.0f, -1.0f or 2345.56f ????
    try {
    	h = new Hashtable(233, 23.0f);
    	th.check(true,"test 4");
        }
    catch (Exception e) {th.fail("shouldn't throw an exception -- "+e);}
    try {
    	h = new Hashtable(233, 0.0f);
	th.fail("should throw an IllegalArgumentException");
	}
    catch (IllegalArgumentException ie) { th.check(true,"test 5");}
    try {
    	h = new Hashtable(233 ,-1.0f);
	th.fail("should throw an IllegalArgumentException");
	}
    catch (IllegalArgumentException ie) { th.check(true,"test 6");}

    h = new Hashtable(buildknownHt());
    th.check (h.size() == 11 , "the map had 11 enries");
    try {
    	h = new Hashtable(null);
    	th.fail("should throw a NullPointerException");
        }
    catch (NullPointerException ne) {th.check(true);}

  }
/**
* implemented.
*
*/
  public void test_elements(){
    th.checkPoint("elements()java.util.Enumeration");
    Hashtable ht = buildknownHt();
    Object o;
    Float f;
    Enumeration e = (Enumeration) ht.elements();
    int i = 0;
    while (e.hasMoreElements()){
    i++;  	
    f= (Float) e.nextElement();
    o = ht.get( f );
    th.check( o != null,"each element is unique -- nr "+i);
    ht.remove(f);
    }
    th.check(i == 11, "we should have 11 elements");
    th.check(ht.size() == 0);
    e = new Hashtable().elements();
    th.check( e != null , "elements should return a non-null value");
    th.check(!e.hasMoreElements(), "e should not have elements");
   }
/**
* implemented.
*
*/
  public void test_get(){
    th.checkPoint("get(java.lang.Object)java.lang.Object");
    Hashtable hte=new Hashtable(),ht = buildknownHt();
    try	{
    	ht.get(null);
    	th.fail("should throw NullPointerException");
    	}
    catch (NullPointerException ne) { th.check(true); }

    th.check( ht.get(new Object()) == null ,"gives back null if not in -- 1");
    Float f = (Float) ht.elements().nextElement();
    Float g = new Float(f.floatValue()+0.00001);
    th.check( ht.get(g) == null ,"gives back null if not in -- 2");
    th.check( ht.get(f) == f,"key and element are same so get(f)==f -- 1");
    ht.put(hte,hte); 	hte.put(f,f);	hte.put(g,g);
    th.check( ht.get(hte) == hte , "changing the hashcode of a key --> key must be found");
    	
    }
/**
* implemented.
*
*/
  public void test_keys(){
    th.checkPoint("keys()java.util.Enumeration");
    Hashtable ht = buildknownHt();
    Object o;
    Float f;
    Enumeration e = (Enumeration) ht.keys();
    int i = 0;
    while (e.hasMoreElements()){
    i++;  	
    f= (Float) e.nextElement();
    o = ht.get( f );
    th.check( o != null,"each key is unique -- nr "+i);
    ht.remove(f);
    }
    th.check(i == 11, "we should have 11 key");
    th.check(ht.size() == 0);
    e = new Hashtable().keys();
    th.check( e != null , "keys should return a non-null value");
    th.check(!e.hasMoreElements(), "e should not have keys");
    ht = new Hashtable();
    e = ht.keys();
    th.check(! e.hasMoreElements() , "empty HT Enum has no elements");
    ht.put("abcd","value");
    e = ht.keys();
    th.check(e.hasMoreElements() , "HT Enum stil has elements");
    th.check("abcd".equals(e.nextElement()) ,"checking returned value");
    th.check(! e.hasMoreElements() , "HT Enum enumerated all elements");

   }
/**
* implemented.
*
*/
  public void test_contains(){
    th.checkPoint("contains(java.lang.Object)boolean");
    Hashtable ht= buildknownHt();
    Float f = new Float(10.0);
    th.check(ht.contains( f ),"contains uses equals -- 1");
    f = new Float(11.0);
    th.check(!ht.contains( f ),"contains uses equals -- 2");
    Double d = new Double(5.0);
    th.check(!ht.contains( d ),"contains uses equals -- 3");
    ht.put(f,d);
    th.check(ht.contains( d ),"contains uses equals -- 4");
    try { ht.contains(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }
  }
/**
* implemented.
*
*/
  public void test_containsKey(){
    th.checkPoint("containsKey(java.lang.Object)boolean");
    Hashtable ht= buildknownHt();
    Float f = new Float(10.0);
    th.check(ht.containsKey( f ),"containsKey uses equals -- 1");
    f = new Float(11.0);
    th.check(!ht.containsKey( f ),"containsKey uses equals -- 2");
    Double d = new Double(5.0);
    th.check(!ht.containsKey( d ),"containsKey uses equals -- 3");
    ht.put(d,f);
    th.check(ht.containsKey( d ),"containsKey uses equals -- 4");
    try { ht.containsKey(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }

    }
/**
* implemented.
*
*/
  public void test_containsValue(){
    th.checkPoint("containsValue(java.lang.Object)boolean");
    Hashtable ht= buildknownHt();
    Float f = new Float(10.0);
    th.check(ht.containsValue( f ),"containsValue uses equals -- 1");
    f = new Float(11.0);
    th.check(!ht.containsValue( f ),"containsValue uses equals -- 2");
    Double d = new Double(5.0);
    th.check(!ht.containsValue( d ),"containsValue uses equals -- 3");
    ht.put(d,f);
    th.check(!ht.containsValue( d ),"containsValue uses equals -- 4");
    d = new Double(89.0);
    ht.put(f,d);
    th.check(ht.containsValue( d ),"containsValue uses equals -- 5");
    try { ht.containsValue(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }

    }
/**
* implemented.
*
*/
  public void test_isEmpty(){
    th.checkPoint("isEmpty()boolean");
    Hashtable ht= buildknownHt();
    th.check(!ht.isEmpty(), "ht is not empty -- 1");
    ht.clear();
    th.check(ht.isEmpty(),"hashtable should be empty --> after clear");
    ht.put(new Object(),ht);
    th.check(!ht.isEmpty(), "ht is not empty -- 2");

    }
/**
* implemented.
*
*/
  public void test_size(){
    th.checkPoint("size()int");
    Hashtable ht= buildknownHt();
    th.check( ht.size() == 11 );
    }

/**
* implemented.
*
*/
  public void test_clear(){
    th.checkPoint("clear()void");
    Hashtable ht = new Hashtable();
    ht.clear();
    ht = buildknownHt();
    if (!ht.isEmpty())
    { ht.clear();
      th.check(ht.isEmpty(),"hashtable should be empty --> after clear");
      try { ht.clear(); //shouldnot throw any exception
            th.check(ht.isEmpty(),"hashtable should be empty --> after 2nd clear");
          }
      catch (Exception e) { th.fail("clear should not throw "+e); }	
    }
  }

/**
* implemented.
*
*/
  public void test_put(){
    th.checkPoint("put(java.lang.Object,java.lang.Object)java.lang.Object");
    Hashtable h = buildknownHt();
    Float f = new Float(33.0f);
    Double d = new Double(343.0);
    th.check( h.put(f,f) == null ,"key f in not used");
    th.check( h.get(f) == f, "make sure element is put there -- 1");
    th.check( h.put(f,d) == f ,"key f in used --> return old element");
    th.check( h.get(f) == d, "make sure element is put there -- 2");

    try { h.put(null, d);
          th.fail("should throw NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check(true); }

    try { h.put(d,null);
          th.fail("should throw NullPointerException -- 2");
        }
    catch (NullPointerException ne) { th.check(true); }

    try { h.put(null,null);
          th.fail("should throw NullPointerException -- 3");
        }
    catch (NullPointerException ne) { th.check(true); }

    }
/**
*  implemented.	<br>
*  --> needs more testing with objects with Map interface
*/
  public void test_putAll(){
    th.checkPoint("putAll(java.lang.Map)void");

    Hashtable h = new Hashtable();
    h.putAll(buildknownHt());
    th.check(h.size() == 11 && h.equals(buildknownHt()));
    Double d =new Double(34.0); Float f = new Float(2.0);
    h.put(f,d);
    h.putAll(buildknownHt());
    th.check(h.size() == 11 && h.equals(buildknownHt()));
    h.put(d,d);
    h.putAll(buildknownHt());
    th.check(h.size() == 12 && (!h.equals(buildknownHt())));

    try { h.putAll(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }



  }
/**
* implemented.
*
*/
  public void test_remove(){
    th.checkPoint("remove(java.lang.Object)java.lang.Object");
    Hashtable h = buildknownHt();
    Float f = new Float(33.0f);
    int i= h.size();
    try { h.remove(null);
          th.fail("should throw NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check(true); }

    th.check(h.remove(f) == null, "key not there so return null");
    th.check(h.size() == i, "check on size -- 1");
    for (int j=0 ; j < 11 ; j++)
    	{
    	f = new Float((float)j);
    	th.check(h.remove(f).equals(f), "key is there so return element -- "+j);
    	th.check(h.size() == --i, "check on size after removing -- "+j);    	
    	}
    }

/**
* implemented.
*
*/
  public void test_entrySet(){
    th.checkPoint("entrySet()java.util.Set");
    Hashtable h = buildknownHt();
    Set s = h.entrySet();
    int j;
    Map.Entry m;
    th.check(s.size() == 11);
    Object [] ao = s.toArray();
    Iterator i = s.iterator();
    for (j =0 ; true ; j++) {
     	if (!i.hasNext()) break;
     	m = (Map.Entry)i.next();
     	if (j==50) break;
    }
    th.check( j == 11 , "Iterator of Set must not do an Inf Loop, got j"+j);
    }

/**
* implemented.
*
*/
  public void test_keySet(){
    th.checkPoint("keySet()java.util.Set");
    Hashtable h = buildknownHt();
    Set s = h.keySet();
    th.check(s.size() == 11);
    for (int i = 0; i < 11 ; i++)
    	{
        th.check(s.contains(new Float((float)i)),"check if all keys are given -- "+i);
    	}
    }

/**
* implemented.
*
*/
  public void test_values(){
    th.checkPoint("values()java.util.Collection");
    Hashtable h = buildknownHt();
    Collection c = h.values();
    th.check(c.size() == 11);
    for (int i = 0; i < 11 ; i++)
    	{
        th.check(c.contains(new Float((float)i)),"check if all values are given -- "+i);
    	}

    }

/**
* implemented.
*
*/
  public void test_clone(){
    th.checkPoint("clone()java.lang.Object");
    Hashtable ht2,ht1 = buildknownHt();
    ht2 = (Hashtable) ht1.clone();
    th.check( ht2.size() == 11 ,"checking size -- got: "+ht2.size());
    th.check( ht2.equals( ht1) ,"clone gives back equal hashtables");
    Object o;
    Float f;
    Enumeration e = (Enumeration) ht1.elements();
    for (int i=0; i < 11; i++) {
    	f= (Float) e.nextElement();
    	o = ht2.get( f );
    	th.check( f == (Float) o,"key and element are the same");
    }
    f= (Float) ht1.elements().nextElement();
    ht2.remove(f);
    th.check(ht1.size() == 11 , "changes in clone do not affect original");
    ht1.put(ht2,ht1);
    th.check(ht2.size() == 10 , "changes in original do not affect clone");

    ht1 =new Hashtable();
    ht2 = (Hashtable) ht1.clone();
    th.check(ht2.size() == 0 , "cloning an empty hashtable must work");

  }

/**
* implemented.
*
*/
  public void test_equals(){
    th.checkPoint("equals(java.lang.Object)boolean");
    Hashtable h2= buildknownHt(),h1 = buildknownHt();
    th.check(h2.equals(h1),"hashtables are equal -- 1");
    h2.remove(new Float(2.0f));
    th.check(!h2.equals(h1),"hashtables are not equal");
    h1.remove(new Float(2.0f));
    th.check(h2.equals(h1),"hashtables are equal -- 2");
    th.check(!h2.equals(new Float(3.0)),"hashtables is not equal to Float");

    }

/**
* implemented.
*
*/
  public void test_hashCode(){
    th.checkPoint("hashCode()int");
    Hashtable h = new Hashtable(13);
    th.check( buildknownHt().hashCode() == buildknownHt().hashCode() );
    Integer i = new Integer(4545);
    String s  = new String("string");
    Double d = new Double(23245.6);
    Object o = new Object();
    h.put(i,s);
    th.check(h.hashCode() == (i.hashCode() ^ s.hashCode()));
    h.put(d,o);
    th.check(h.hashCode() == (i.hashCode() ^ s.hashCode())+(d.hashCode() ^ o.hashCode()));

    }

/**
* implemented.
*
*/
  public void test_toString(){
    th.checkPoint("toString()java.lang.String");
    Hashtable h = new Hashtable(13,0.75f);
    th.check(h.toString().equals("{}"), "got: "+h);
    h.put("SmartMove","Fantastic");
    th.check(h.toString().equals("{SmartMove=Fantastic}"), "got: "+h);
    h.put("nr 1",new Float(23.0));
    // the order is not specified
    th.check(h.toString().equals("{SmartMove=Fantastic, nr 1=23.0}")||
    	h.toString().equals("{nr 1=23.0, SmartMove=Fantastic}"), "got: "+h);
    h.remove("SmartMove");
    th.check(h.toString().equals("{nr 1=23.0}"), "got: "+h);
    }
/**
* implemented.
*
*/
  public void test_rehash(){
    th.checkPoint("rehash()void");
    Hashtable h = new Hashtable(3 , 0.5f);
    try {
    	h.put("Smart","Move");
    	h.put("rehash","now");
    	th.check(h.size() == 2);
        }
     catch (Exception e) {th.fail("caught exception "+e);}

    }
/**
* the goal of this test is to see how the hashtable behaves if we do a lot put's and removes. <br>
* we perform this test for different loadFactors and a low initial size <br>
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
    do_behaviourtest(0.2f);
    do_behaviourtest(0.70f);
    do_behaviourtest(0.75f);
    do_behaviourtest(0.95f);
    do_behaviourtest(1.0f);

    }
  protected void sleep(int time){
  	try { Thread.sleep(time); }
  	catch (Exception e) {}	
  }

  protected void check_presence(Hashtable h){
    th.check( h.get(st) != null, "checking presence st -- sequence "+sqnce);
    th.check( h.get(sh) != null, "checking presence sh -- sequence "+sqnce);
    th.check( h.get(i) != null, "checking presence i -- sequence "+sqnce);
    th.check( h.get(b) != null, "checking presence b -- sequence "+sqnce);
    th.check( h.get(l) != null, "checking presence l -- sequence "+sqnce);
    sqnce++;
  }

  protected void do_behaviourtest(float loadFactor) {

    th.checkPoint("behaviour testing with loadFactor "+loadFactor);
    Hashtable h = new Hashtable(11 , loadFactor);
    int j=0;
    Float f;
    h.put(st,"a"); h.put(b,"byte"); h.put(sh,"short"); h.put(i,"int"); h.put(l,"long");
    check_presence(h);
    sqnce = 1;
    for ( ; j < 100 ; j++ )
    {   f = new Float((float)j);
        h.put(f,f);
    }
    th.check(h.size() == 105,"size checking -- 1 got: "+h.size());
    check_presence(h);
    for ( ; j < 200 ; j++ )
    {   f = new Float((float)j);
        h.put(f,f);
    }
    th.check(h.size() == 205,"size checking -- 2 got: "+h.size());
    check_presence(h);

    for ( ; j < 300 ; j++ )
    {   f = new Float((float)j);
        h.put(f,f);
    }
    th.check(h.size() == 305,"size checking -- 3 got: "+h.size());
    check_presence(h);
// replacing values -- checking if we get a non-zero value
    th.check("a".equals(h.put(st,"na")), "replacing values -- 1 - st");
    th.check("byte".equals(h.put(b,"nbyte")), "replacing values -- 2 - b");
    th.check("short".equals(h.put(sh,"nshort")), "replacing values -- 3 -sh");
    th.check("int".equals(h.put(i,"nint"))  , "replacing values -- 4 -i");
    th.check("long".equals(h.put(l,"nlong")), "replacing values -- 5 -l");


    for ( ; j > 199 ; j-- )
    {   f = new Float((float)j);
        h.remove(f);
    }
    th.check(h.size() == 205,"size checking -- 4 got: "+h.size());
    check_presence(h);
    for ( ; j > 99 ; j-- )
    {   f = new Float((float)j);
        h.remove(f);
    }
    th.check(h.size() == 105,"size checking -- 5 got: "+h.size());
    check_presence(h);
    for ( ; j > -1 ; j-- )
    {   f = new Float((float)j);
        h.remove(f);
    }
    th.check(h.size() == 5  ,"size checking -- 6 got: "+h.size());

    th.debug(h.toString());
    check_presence(h);

    }
}
