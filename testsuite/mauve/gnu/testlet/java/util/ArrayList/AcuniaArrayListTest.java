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

package gnu.testlet.java.util.ArrayList;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

/**
*  Written by ACUNIA. <br>
*                        <br>
*  this file contains test for ArrayList   <br>
*  <br>
*  It might be usefull to find out how the capacity evolves <br>
*  --> this must be done with reflection ... (not done yet)
*/
public class AcuniaArrayListTest extends ArrayList implements Testlet
{
  protected TestHarness th;

  public AcuniaArrayListTest() { /* Empty constructor needed for TestLet */}

  public void test (TestHarness harness)
    {
       th = harness;
       test_ArrayList();
       test_get();
       test_ensureCapacity();
       test_trimToSize();
       test_add();
       test_addAll();
       test_clear();
       test_remove();
       test_set();
       test_contains();
       test_isEmpty();
       test_indexOf();
       test_size();
       test_lastIndexOf();
       test_toArray();
       test_clone();
       // extra
       test_removeRange();
       test_MC_iterator();

     }

  protected ArrayList buildAL() {
    Vector v = new Vector();
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    return new ArrayList(v);

  }


/**
*   not implemented. <br>
*   only ArrayList(Collection c) is tested
*/
  public void test_ArrayList(){
    th.checkPoint("arrayList(java.util.Collection)");
    Vector v = new Vector();
    ArrayList al = new ArrayList(v);
    th.check( al.isEmpty() , "no elements added");
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    al = new ArrayList(v);
    th.check(v.equals(al) , "check if everything is OK");
    try {
    	new ArrayList(null);
    	th.fail("should throw a NullPointerException");
    	}
    catch(NullPointerException npe) { th.check(true); }
  }

/**
* implemented. <br>
*
*/
  public void test_get(){
    th.checkPoint("get(int)java.lang.Object");
    ArrayList al = new ArrayList();
    try {
        al.get(0);
        th.fail("should throw an IndexOutOfBoundsException -- 1");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    try {
        al.get(-1);
        th.fail("should throw an IndexOutOfBoundsException -- 2");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    al = buildAL();
    try {
        al.get(14);
        th.fail("should throw an IndexOutOfBoundsException -- 3");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    try {
        al.get(-1);
        th.fail("should throw an IndexOutOfBoundsException -- 4");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    th.check("a".equals(al.get(0)) , "checking returnvalue -- 1");
    th.check("c".equals(al.get(1)) , "checking returnvalue -- 2");
    th.check("u".equals(al.get(2)) , "checking returnvalue -- 3");
    th.check("a".equals(al.get(5)) , "checking returnvalue -- 4");
    th.check("a".equals(al.get(7)) , "checking returnvalue -- 5");
    th.check("c".equals(al.get(8)) , "checking returnvalue -- 6");
    th.check("u".equals(al.get(9)) , "checking returnvalue -- 7");
    th.check("a".equals(al.get(12)), "checking returnvalue -- 8");
    th.check( null == al.get(6)    , "checking returnvalue -- 9");
    th.check( null == al.get(13)   , "checking returnvalue -- 10");
  }

/**
* implemented. <br>
* => might need extra testing --> using reflection ...
*/
  public void test_ensureCapacity(){
    th.checkPoint("ensureCapacity(int)");
    ArrayList al = buildAL();
    al.ensureCapacity(4);
    th.check(al.size() == 14 , "make sure the list cannot be downsized !");

  }
/**
*   not implemented. <br>
*
*/
  public void test_trimToSize(){
    th.checkPoint("trimToSize()");

  }
/**
* implemented. <br>
*
*/
  public void test_add(){
    th.checkPoint("add(int,java.lang.Object)void");
    ArrayList al = new ArrayList();
    try {
        al.add(-1,"a");
        th.fail("should throw an IndexOutOfBoundsException -- 1");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    try {
        al.add(1,"a");
        th.fail("should throw an IndexOutOfBoundsException -- 2");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    al.add(0,"a");
    al.add(1,"c");
    al.add(2,"u");
    al.add(1,null);
    th.check("a".equals(al.get(0))&& null==al.get(1) && "c".equals(al.get(2)) && "u".equals(al.get(3)) , "checking add ...");

    th.checkPoint("add(java.lang.Object)boolean");
    al = new ArrayList();
    th.check(al.add("a") , "checking return value -- 1");
    th.check(al.add("c") , "checking return value -- 2");
    th.check(al.add("u") , "checking return value -- 3");
    th.check(al.add("n") , "checking return value -- 4");
    th.check(al.add("i") , "checking return value -- 5");
    th.check(al.add("a") , "checking return value -- 6");
    th.check(al.add(null) , "checking return value -- 7");
    th.check(al.add("end") , "checking return value -- 8");
    th.check("a".equals(al.get(0))&& null==al.get(6) && "c".equals(al.get(1)) && "u".equals(al.get(2)) , "checking add ... -- 1");
    th.check("a".equals(al.get(5))&& "end".equals(al.get(7)) && "n".equals(al.get(3)) && "i".equals(al.get(4)) , "checking add ... -- 2");

  }
/**
* implemented. <br>
*
*/
  public void test_addAll(){
    th.checkPoint("addAll(java.util.Collection)boolean");
    ArrayList al =new ArrayList();
    try { al.addAll(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }
    Collection c = (Collection) al;
    th.check(!al.addAll(c) ,"checking returnvalue -- 1");
    al.add("a"); al.add("b"); al.add("c");
    c = (Collection) al;
    al = buildAL();
    th.check(al.addAll(c) ,"checking returnvalue -- 2");
    th.check(al.containsAll(c), "extra on containsAll -- 1");
    th.check(al.get(14)=="a" && al.get(15)=="b" && al.get(16)=="c", "checking added on right positions");

    th.checkPoint("addAll(int,java.util.Collection)boolean");
    al =new ArrayList();
    c = (Collection) al;
    th.check(!al.addAll(0,c) ,"checking returnvalue -- 1");
    al.add("a"); al.add("b"); al.add("c");
    c = (Collection) al;
    al = buildAL();
    try { al.addAll(-1,c);
          th.fail("should throw exception -- 1");
        }
    catch (IndexOutOfBoundsException ae) { th.check(true); }
    try { al.addAll(15,c);
          th.fail("should throw exception -- 2");
        }
    catch (IndexOutOfBoundsException ae) { th.check(true); }
    try { th.check(al.addAll(11,c),"checking returnvalue -- 2"); }
    catch (ArrayIndexOutOfBoundsException ae) { th.fail("shouldn't throw exception -- 1"); }
    th.check(al.containsAll(c), "extra on containsAll -- 1");
    th.check(al.get(11)=="a" && al.get(12)=="b" && al.get(13)=="c", "checking added on right positions -- 1");
    th.check(al.addAll(1,c),"checking returnvalue -- 3");
    th.check(al.get(1)=="a" && al.get(2)=="b" && al.get(3)=="c", "checking added on right positions -- 2");

  }
/**
* implemented. <br>
*
*/
  public void test_clear(){
    th.checkPoint("clear()void");
    ArrayList al = new ArrayList();
    al.clear();
    al = buildAL();
    al.clear();
    th.check(al.size()== 0 && al.isEmpty() , "list is empty ...");


  }
/**
* implemented. <br>
*
*/
  public void test_remove(){
    th.checkPoint("remove(int)java.lang.Object");
    ArrayList al = buildAL();
    try {
    	al.remove(-1);
	th.fail("should throw an IndexOutOfBoundsException -- 1" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    try {
    	al.remove(14);
	th.fail("should throw an IndexOutOfBoundsException -- 2" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    th.check( "a".equals(al.remove(5)) , "checking returnvalue remove -- 1");
    th.check("a".equals(al.get(0))&& null==al.get(5) && "c".equals(al.get(1)) && "u".equals(al.get(2)) , "checking remove ... -- 1");
    th.check("a".equals(al.get(6))&& "c".equals(al.get(7)) && "n".equals(al.get(3)) && "i".equals(al.get(4)) , "checking remove ... -- 2");
    th.check(al.size() == 13 , "checking new size -- 1");   	
    th.check( al.remove(5) == null , "checking returnvalue remove -- 2");
    th.check(al.size() == 12 , "checking new size -- 2");   	
    th.check( al.remove(11) == null, "checking returnvalue remove -- 3");
    th.check( "a".equals(al.remove(0)) , "checking returnvalue remove -- 4");
    th.check( "u".equals(al.remove(1)) , "checking returnvalue remove -- 5");
    th.check( "i".equals(al.remove(2)) , "checking returnvalue remove -- 6");
    th.check( "a".equals(al.remove(2)) , "checking returnvalue remove -- 7");
    th.check( "u".equals(al.remove(3)) , "checking returnvalue remove -- 8");
    th.check( "a".equals(al.remove(5)) , "checking returnvalue remove -- 9");
    th.check( "i".equals(al.remove(4)) , "checking returnvalue remove -- 10");
    th.check( "c".equals(al.get(0))&& "c".equals(al.get(2)) && "n".equals(al.get(3)) && "n".equals(al.get(1)) , "checking remove ... -- 3");
    th.check(al.size() == 4 , "checking new size -- 3");   	
    al.remove(0);
    al.remove(0);
    al.remove(0);
    al.remove(0);
    th.check(al.size() == 0 , "checking new size -- 4");   	

    al = new ArrayList();
    try {
    	al.remove(0);
	th.fail("should throw an IndexOutOfBoundsException -- 3" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }


  }
/**
* implemented. <br>
*
*/
  public void test_set(){
    th.checkPoint("set(int,java.lang.Object)java.lang.Object");
    ArrayList al = new ArrayList();
    try {
    	al.set(-1,"a");
	th.fail("should throw an IndexOutOfBoundsException -- 1" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    try {
    	al.set(0,"a");
	th.fail("should throw an IndexOutOfBoundsException -- 2" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    al = buildAL();
    try {
    	al.set(-1,"a");
	th.fail("should throw an IndexOutOfBoundsException -- 3" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    try {
    	al.set(14,"a");
	th.fail("should throw an IndexOutOfBoundsException -- 4" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    th.check( "a".equals(al.set(5,"b")) , "checking returnvalue of set -- 1");
    th.check( "a".equals(al.set(0,null)), "checking returnvalue of set -- 2");
    th.check( "b".equals(al.get(5)), "checking effect of set -- 1");
    th.check( al.get(0) == null    , "checking effect of set -- 2");
    th.check( "b".equals(al.set(5,"a")), "checking returnvalue of set -- 3");
    th.check( al.set(0,null) == null   , "checking returnvalue of set -- 4");
    th.check( "a".equals(al.get(5)), "checking effect of set -- 3");
    th.check( al.get(0) == null    , "checking effect of set -- 4");

  }
/**
* implemented. <br>
*
*/
  public void test_contains(){
    th.checkPoint("contains(java.lang.Object)boolean");
    ArrayList al = new ArrayList();
    th.check(!al.contains(null),"checking empty List -- 1");
    th.check(!al.contains(al)  ,"checking empty List -- 2");
    al = buildAL();
    th.check( al.contains(null), "check contains ... -- 1");
    th.check( al.contains("a") , "check contains ... -- 2");
    th.check( al.contains("c") , "check contains ... -- 3");
    th.check(!al.contains(this), "check contains ... -- 4");
    al.remove(6);
    th.check( al.contains(null), "check contains ... -- 5");
    al.remove(12);
    th.check(!al.contains(null), "check contains ... -- 6");
    th.check(!al.contains("b") , "check contains ... -- 7");
    th.check(!al.contains(al)  , "check contains ... -- 8");
  }
/**
* implemented. <br>
*
*/
  public void test_isEmpty(){
    th.checkPoint("isEmpty()boolean");
    ArrayList al = new ArrayList();
    th.check(al.isEmpty() , "checking returnvalue -- 1");
    al.add("A");
    th.check(!al.isEmpty() , "checking returnvalue -- 2");
    al.remove(0);
    th.check(al.isEmpty() , "checking returnvalue -- 3");
  }
/**
* implemented. <br>
*
*/
  public void test_indexOf(){
    th.checkPoint("indexOf(java.lang.Object)int");
    ArrayList al = new ArrayList();
    th.check( al.indexOf(null)== -1,"checks on empty list -- 1");
    th.check( al.indexOf(al)== -1 , "checks on empty list -- 2");
    Object o = new Object();
    al =buildAL();
    th.check( al.indexOf(o) == -1 , " doesn't contain -- 1");
    th.check( al.indexOf("a") == 0 , "contains -- 2");
    th.check( al.indexOf(o) == -1, "contains -- 3");
    al.add(9,o);
    th.check( al.indexOf(o) == 9 , "contains -- 4");
    th.check( al.indexOf(new Object()) == -1 , "doesn't contain -- 5");
    th.check(al.indexOf(null) == 6, "null was added to the Vector");
    al.remove(6);
    th.check(al.indexOf(null) == 13, "null was added twice to the Vector");
    al.remove(13);
    th.check(al.indexOf(null) == -1, "null was removed to the Vector");
    th.check( al.indexOf("c") == 1 , "contains -- 6");
    th.check( al.indexOf("u") == 2 , "contains -- 7");
    th.check( al.indexOf("n") == 3 , "contains -- 8");
    	
  }
/**
* implemented. <br>
*
*/
  public void test_size(){
    th.checkPoint("size()int");
    ArrayList al = new ArrayList();
    th.check( al.size() == 0 , "check on size -- 1");
    al.addAll(buildAL());
    th.check( al.size() == 14 , "check on size -- 1");
    al.remove(5);
    th.check( al.size() == 13 , "check on size -- 1");
    al.add(4,"G");
    th.check( al.size() == 14 , "check on size -- 1");

  }
/**
* implemented. <br>
*
*/
  public void test_lastIndexOf(){
    th.checkPoint("lastIndexOf(java.lang.Object)int");
    ArrayList al = new ArrayList();
    th.check( al.lastIndexOf(null)== -1,"checks on empty list -- 1");
    th.check( al.lastIndexOf(al)== -1 , "checks on empty list -- 2");
    Object o = new Object();
    al =buildAL();
    th.check( al.lastIndexOf(o) == -1 , " doesn't contain -- 1");
    th.check( al.lastIndexOf("a") == 12 , "contains -- 2");
    th.check( al.lastIndexOf(o) == -1, "contains -- 3");
    al.add(9,o);
    th.check( al.lastIndexOf(o) == 9 , "contains -- 4");
    th.check( al.lastIndexOf(new Object()) == -1 , "doesn't contain -- 5");
    th.check( al.lastIndexOf(null) == 14, "null was added to the Vector");
    al.remove(14);
    th.check( al.lastIndexOf(null) == 6 , "null was added twice to the Vector");
    al.remove(6);
    th.check( al.lastIndexOf(null) == -1, "null was removed to the Vector");
    th.check( al.lastIndexOf("c") == 7 , "contains -- 6, got "+al.lastIndexOf("c"));
    th.check( al.lastIndexOf("u") == 9 , "contains -- 7, got "+al.lastIndexOf("u"));
    th.check( al.lastIndexOf("n") == 10, "contains -- 8, got "+al.lastIndexOf("n"));

  }
/**
* implemented. <br>
*
*/
  public void test_toArray(){
   th.checkPoint("toArray()[java.lang.Object");
    ArrayList v = new ArrayList();
    Object o[] = v.toArray();
    th.check(o.length == 0 , "checking size Object array");
    v.add("a"); v.add(null); v.add("b");
    o = v.toArray();
    th.check(o[0]== "a" && o[1] == null && o[2] == "b" , "checking elements -- 1");
    th.check(o.length == 3 , "checking size Object array");

  th.checkPoint("toArray([java.lang.Object)[java.lang.Object");
    v = new ArrayList();
    try { v.toArray(null);
          th.fail("should throw NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check(true); }
    v.add("a"); v.add(null); v.add("b");
    String sa[] = new String[5];
    sa[3] = "deleteme"; sa[4] = "leavemealone";
    th.check(v.toArray(sa) == sa , "sa is large enough, no new array created");
    th.check(sa[0]=="a" && sa[1] == null && sa[2] == "b" , "checking elements -- 1"+sa[0]+", "+sa[1]+", "+sa[2]);
    th.check(sa.length == 5 , "checking size Object array");
    th.check(sa[3]==null && sa[4]=="leavemealone", "check other elements -- 1"+sa[3]+", "+sa[4]);
    v = buildAL();
    try { v.toArray(null);
          th.fail("should throw NullPointerException -- 2");
        }
    catch (NullPointerException ne) { th.check(true); }
    try { v.toArray(new Class[5]);
          th.fail("should throw an ArrayStoreException");
        }
    catch (ArrayStoreException ae) { th.check(true); }
    v.add(null);
    String sar[];
    sa = new String[15];
    sar = (String[])v.toArray(sa);
    th.check( sar == sa , "returned array is the same");

  }
/**
* implemented. <br>
*
*/
  public void test_clone(){
    th.checkPoint("clone()java.lang.Object");
    ArrayList cal,al = new ArrayList();
    cal = (ArrayList)al.clone();
    th.check(cal.size() == 0, "checking size -- 1");
    al.add("a")	;al.add("b")    ;al.add("c"); al.add(null);
    cal = (ArrayList)al.clone();
    th.check(cal.size() == al.size(), "checking size -- 2");
    th.check( al != cal , "Objects are not the same");
    th.check( al.equals(cal) , "cloned list is equal");
    al.add("a");
    th.check(cal.size() == 4, "changes in one object doen't affect the other -- 2");

  }

/**
* implemented. <br>
*
*/
  public void test_removeRange(){
    th.checkPoint("removeRange(int,int)void");
    AcuniaArrayListTest xal = new AcuniaArrayListTest(buildAL());
    ArrayList al = buildAL();
    xal.ensureCapacity(40);
    try {
    	xal.removeRange(0,-1);
        th.fail("should throw an IndexOutOfBoundsException -- 1");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    th.check(xal.equals(al) , "ArrayList must not be changed -- 1");

    try {
    	xal.removeRange(-1,2);
        th.fail("should throw an IndexOutOfBoundsException -- 2");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    th.check(xal.equals(al) , "ArrayList must not be changed -- 2");
    try {
    	xal.removeRange(3,2);
        th.fail("should throw an IndexOutOfBoundsException -- 3");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    try {
    	th.check(al.equals(xal) , "ArrayList must not be changed -- 3");
    }
    catch (Exception e) { th.debug("bad operations messed up ArrayList"); }
    xal = new AcuniaArrayListTest(buildAL());
    xal.ensureCapacity(40);
    try {
    	xal.removeRange(3,15);
        th.fail("should throw an IndexOutOfBoundsException -- 4");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    th.check(xal.equals(al) , "ArrayList must not be changed -- 4");
    xal = new AcuniaArrayListTest(buildAL());
    xal.ensureCapacity(40);
    try {
    	xal.removeRange(15,13);
        th.fail("should throw an IndexOutOfBoundsException -- 5");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    try {
    	th.check(xal.equals(al) , "ArrayList must not be changed -- 5");
    }
    catch (Exception e) { th.debug("bad operations messed up ArrayList"); }
    xal = new AcuniaArrayListTest(buildAL());
    xal.ensureCapacity(40);
    xal.removeRange(14,14);
    th.check(xal.size() == 14 , "no elements should have been removed -- 6, size = "+xal.size());
    xal.removeRange(10,14);
    th.check(xal.size() == 10 , "4 elements should have been removed");
    th.check( "a".equals(xal.get(0)) && "a".equals(xal.get(5)) && "a".equals(xal.get(7)) ,"check contents -- 1");
    xal.removeRange(2,7);
    th.check(xal.size() == 5 , "5 elements should have been removed");
    th.check( "a".equals(xal.get(0)) && "c".equals(xal.get(1)) && "a".equals(xal.get(2))
                 && "c".equals(xal.get(3)) && "u".equals(xal.get(4)) ,"check contents -- 2");
    xal.removeRange(0,2);
    th.check( "a".equals(xal.get(0)) && "c".equals(xal.get(1)) && "u".equals(xal.get(2)) ,"check contents -- 3");
    th.check(xal.size() == 3 , "2 elements should have been removed");
  }

/**
* implemented. <br>
*
*/
  public void test_MC_iterator(){
    th.checkPoint("ModCount(in)iterator");
    AcuniaArrayListTest xal = new AcuniaArrayListTest(buildAL());
    Iterator it = xal.iterator();
    xal.removeRange(1,10);
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 1");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }
    ArrayList al = buildAL();
    it = al.iterator();
    al.trimToSize();
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 3");
    } catch(ConcurrentModificationException ioobe) { th.check(true); }

    it = al.iterator();
    al.ensureCapacity(25);
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 3");
    } catch(ConcurrentModificationException ioobe) { th.check(true); }

    it = al.iterator();
    al.get(0);
    al.contains(null);
    al.isEmpty();
    al.indexOf(null);
    al.lastIndexOf(null);
    al.size();
    al.toArray();
    al.toArray(new String[10]);
    al.clone();
    try {
    	it.next();
    	th.check(true);
        }
    catch(ConcurrentModificationException ioobe) { th.fail("should not throw a ConcurrentModificationException -- 2"); }

    it = al.iterator();
    al.add("b");
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 3");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }

    it = al.iterator();
    al.add(3,"b");
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 4");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }

    it = al.iterator();
    al.addAll(xal);
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 5");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }

    it = al.iterator();
    al.addAll(2,xal);
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 6");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }

    it = al.iterator();
    al.remove(2);
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 8");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }

    it = al.iterator();
    al.clear();
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 9");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }
  }

// The following fields and methods are used in tests that need an ArrayList.

        private boolean didRemoveRange=false;
        private int from = -1;
        private int to   = -1;
        public AcuniaArrayListTest(Collection c){
                super(c);
        }

        public void removeRange(int fidx, int tidx) {
                didRemoveRange=true;
                to   = tidx;
                from = fidx;
                super.removeRange(fidx, tidx);
        }

        public boolean get_dRR() {
                return didRemoveRange;
        }
        public void set_dRR(boolean b) {
                didRemoveRange = b;
        }
        public int get_to() {
                return to;
        }
        public int get_from() {
                return from;
        }

}
