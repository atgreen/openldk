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

package gnu.testlet.java.util.LinkedList;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

/**
*  Written by ACUNIA. <br>
*                        <br>
*  this file contains test for LinkedList   <br>
*  <br>
*/
public class AcuniaLinkedListTest implements Testlet
{
  protected TestHarness th;

  public void test (TestHarness harness)
    {
       th = harness;
       test_LinkedList();
       test_add();
       test_addAll();
       test_clear();
       test_remove();
       test_set();
       test_contains();
       test_indexOf();
       test_size();
       test_lastIndexOf();
       test_toArray();
       test_clone();
       test_iterator();
       test_getFirst();
       test_getLast();
       test_addFirst();
       test_addLast();
       test_removeFirst();
       test_removeLast();
       test_ListIterator();


     }

  protected LinkedList buildAL() {
    Vector v = new Vector();
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    return new LinkedList(v);

  }


/**
*   implemented. <br>
*   only LinkedList(Collection c) is tested
*/
  public void test_LinkedList(){
    th.checkPoint("LinkedList(java.util.Collection)");
    Vector v = new Vector();
    LinkedList al = new LinkedList(v);
    th.check( al.isEmpty() , "no elements added");
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    al = new LinkedList(v);
//    th.debug(al.toString());
    th.check(v.equals(al) , "check if everything is OK");
    try {
    	new LinkedList(null);
    	th.fail("should throw a NullPointerException");
    	}
    catch(NullPointerException npe) { th.check(true); }
  }


/**
* implemented. <br>
*
*/
  public void test_add(){
    th.checkPoint("add(int,java.lang.Object)void");
    LinkedList al = new LinkedList();
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
    al.clear();
    al.add(0,"a");
    al.add(1,"c");
    al.add(2,"u");
    al.add(1,null);
//    th.debug(al.toString());
    th.check("a".equals(al.get(0))&& null==al.get(1) && "c".equals(al.get(2)) && "u".equals(al.get(3)) , "checking add ...");

    th.checkPoint("add(java.lang.Object)boolean");
    al = new LinkedList();
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
    LinkedList al =new LinkedList();
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
    al =new LinkedList();
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
    LinkedList al = new LinkedList();
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
    LinkedList al = buildAL();
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

    al = new LinkedList();
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
    LinkedList al = new LinkedList();
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
    LinkedList al = new LinkedList();
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
    LinkedList al = new LinkedList();
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
    LinkedList al = new LinkedList();
    th.check( al.indexOf(null)== -1,"checks on empty list -- 1");
    th.check( al.indexOf(al)== -1 , "checks on empty list -- 2");
    Object o = new Object();
    al =buildAL();
    th.check( al.indexOf(o) == -1 , " doesn't contain -- 1");
    th.check( al.indexOf("a") == 0 , "contains -- 2");
    th.check( al.indexOf("Q") == -1, "doesn't contain -- 3");
//    th.debug(al.toString());
    al.add(9,o);
//    th.debug(al.toString());
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
    LinkedList al = new LinkedList();
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
    LinkedList al = new LinkedList();
    th.check( al.lastIndexOf(null)== -1,"checks on empty list -- 1");
    th.check( al.lastIndexOf(al)== -1 , "checks on empty list -- 2");
    Object o = new Object();
    al =buildAL();
//    th.debug(al.toString());
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
    LinkedList v = new LinkedList();
    Object o[] = v.toArray();
    th.check(o.length == 0 , "checking size Object array");
    v.add("a"); v.add(null); v.add("b");
    o = v.toArray();
    th.check(o[0]== "a" && o[1] == null && o[2] == "b" , "checking elements -- 1");
    th.check(o.length == 3 , "checking size Object array");

  th.checkPoint("toArray([java.lang.Object)[java.lang.Object");
    v = new LinkedList();
    try { v.toArray(null);
          th.fail("should throw NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check(true); }
    v.add("a"); v.add(null); v.add("b");
    String sa[] = new String[5];
    sa[3] = "deleteme"; sa[4] = "leavemealone";
//    th.debug(v.toString());
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
    LinkedList cal,al = new LinkedList();
    cal = (LinkedList)al.clone();
    th.check(cal.size() == 0, "checking size -- 1");
    al.add("a")	;al.add("b")    ;al.add("c"); al.add(null);
    cal = (LinkedList)al.clone();
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
  public void test_iterator(){
    th.checkPoint("ModCount(in)iterator");
    LinkedList al = buildAL();
    Iterator it = al.iterator();
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
    al.addAll(buildAL());
    try {
    	it.next();
        th.fail("should throw a ConcurrentModificationException -- 5");
        }
    catch(ConcurrentModificationException ioobe) { th.check(true); }
    it = al.iterator();
    al.addAll(2,buildAL());
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

/**
* implemented. <br>
*
*/
  public void test_getFirst(){
    th.checkPoint("getFirst()java.lang.Object");
    LinkedList ll = new LinkedList();
    try {
    	ll.getFirst();
    	th.fail("should throw a NoSuchElementException");
    }
    catch (NoSuchElementException nsee) { th.check(true, "caught exception");}
    ll = buildAL();
    th.check("a".equals(ll.getFirst()) , "getFirst -- 1");
    ll.removeFirst();
    th.check("c".equals(ll.getFirst()) , "getFirst -- 2");
    ll.addFirst("d");
    th.check("d".equals(ll.getFirst()) , "getFirst -- 3");
  }

/**
* implemented. <br>
*
*/
  public void test_getLast(){
    th.checkPoint("getLast()java.lang.Object");
    LinkedList ll = new LinkedList();
    try {
    	ll.getLast();
    	th.fail("should throw a NoSuchElementException");
    }
    catch (NoSuchElementException nsee) { th.check(true, "caught exception");}
    ll = buildAL();
    th.check(null ==  ll.getLast() , "getLast -- 1");
    ll.removeLast();
    th.check("a".equals(ll.getLast()) , "getLast -- 2");
    ll.addLast("d");
    th.check("d".equals(ll.getLast()) , "getLast -- 3");
  }

/**
* implemented. <br>
*
*/
  public void test_addFirst(){
    th.checkPoint("addFirst(java.lang.Object)void");
    LinkedList ll = new LinkedList();
    ll.addFirst("a");
    th.check("a".equals(ll.getLast()) , "addFirst on empty List -- 1");
    th.check("a".equals(ll.getFirst()) , "addFirst on empty List -- 1");
    ll.addFirst("c");
    th.check("a".equals(ll.getLast()) , "addFirst on List -- 2");
    th.check("c".equals(ll.getFirst()) , "addFirst on List -- 2");
    ll.addFirst(null);
    th.check("a".equals(ll.getLast()) , "addFirst on List -- 3");
    th.check(null == ll.getFirst() , "addFirst on List -- 3");
    th.check(null == ll.get(0)     , "checking order with get -- 1");
    th.check("c".equals(ll.get(1)) , "checking order with get -- 2");
    th.check("a".equals(ll.get(2)) , "checking order with get -- 3");
    th.check(ll.size() == 3 , "checking size increment ...");
  }

/**
* implemented. <br>
*
*/
  public void test_addLast(){
    th.checkPoint("addLast(java.lang.Object)void");
    LinkedList ll = new LinkedList();
    ll.addLast("a");
    th.check("a".equals(ll.getLast()) , "addLast on empty List -- 1");
    th.check("a".equals(ll.getFirst()) , "addLast on empty List -- 1");
    ll.addLast("c");
    th.check("c".equals(ll.getLast()) , "addLast on List -- 2");
    th.check("a".equals(ll.getFirst()) , "addLast on List -- 2");
    ll.addLast(null);
    th.check("a".equals(ll.getFirst()) , "addLast on List -- 3");
    th.check(null == ll.getLast() , "addLast on List -- 3");
    th.check(null == ll.get(2)     , "checking order with get -- 1");
    th.check("c".equals(ll.get(1)) , "checking order with get -- 2");
    th.check("a".equals(ll.get(0)) , "checking order with get -- 3");
    th.check(ll.size() == 3 , "checking size increment ...");
  }

/**
* implemented. <br>
*
*/
  public void test_removeFirst(){
    th.checkPoint("removeFirst()java.lang.Object");
    LinkedList ll = new LinkedList();
    try {
    	ll.removeFirst();
    	th.fail("should throw a NoSuchElementException");
    }
    catch (NoSuchElementException nsee) { th.check(true, "caught exception");}
    ll = buildAL();
    th.check("a".equals(ll.removeFirst()) , "removeFirst -- 1");
    th.check("c".equals(ll.removeFirst()) , "removeFirst -- 2");
    th.check("u".equals(ll.getFirst()) , "changing pointer to first ...");
    th.check("u".equals(ll.removeFirst()) , "removeFirst -- 3");
    th.check(ll.size() == 11 , "checking size decrement ...");
    th.check("n".equals(ll.removeFirst()) , "removeFirst -- 4");
    th.check("i".equals(ll.removeFirst()) , "removeFirst -- 5");
    th.check("a".equals(ll.removeFirst()) , "removeFirst -- 6");
    th.check(null == ll.removeFirst() , "removeFirst -- 7");
    ll.removeFirst();
    ll.removeFirst();
    ll.removeFirst();
    ll.removeFirst();
    ll.removeFirst();
    th.check("a".equals(ll.removeFirst()) , "removeFirst -- 8");
    th.check(null == ll.getFirst() , "removeFirst -- 8");
    th.check(null == ll.getLast() , "removeFirst -- 9");
    th.check(null == ll.removeFirst() , "removeFirst -- 10");
    try {
    	ll.removeFirst();
    	th.fail("should throw a NoSuchElementException");
    }
    catch (NoSuchElementException nsee) { th.check(true, "caught exception");}
  }

/**
* implemented. <br>
*
*/
  public void test_removeLast(){
    th.checkPoint("removeLast()java.lang.Object");
    LinkedList ll = new LinkedList();
    try {
    	ll.removeLast();
    	th.fail("should throw a NoSuchElementException");
    }
    catch (NoSuchElementException nsee) { th.check(true, "caught exception");}
    ll = buildAL();
    th.check(null == ll.removeLast() , "removeLast -- 0");
    th.check("a".equals(ll.removeLast()) , "removeLast -- 1");
    th.check("i".equals(ll.removeLast()) , "removeLast -- 2");
    th.check("n".equals(ll.getLast()) , "changing pointer to last ...");
    th.check("n".equals(ll.removeLast()) , "removeLast -- 3");
    th.check("u".equals(ll.get(9)) , "checking elements -- 3a");
    th.check("c".equals(ll.get(8)) , "checking elements -- 3b");
    th.check(ll.size() == 10 , "checking size decrement ...");
    th.check("u".equals(ll.removeLast()) , "removeLast -- 4");
    th.check("c".equals(ll.removeLast()) , "removeLast -- 5");
    th.check("a".equals(ll.removeLast()) , "removeLast -- 6");
    ll.removeLast();
    ll.removeLast();
    ll.removeLast();
    ll.removeLast();
    ll.removeLast();
//    th.debug(ll.toString());
    th.check("c".equals(ll.removeLast()) , "removeLast -- 7");
//    th.debug(ll.toString());
    th.check("a".equals(ll.getLast()) , "removeLast -- 8");
    th.check("a".equals(ll.getFirst()) , "removeLast -- 9");
    th.check("a".equals(ll.removeLast()) , "removeLast -- 10");
    try {
    	ll.removeLast();
    	th.fail("should throw a NoSuchElementException");
    }
    catch (NoSuchElementException nsee) { th.check(true, "caught exception");}
  }

/**
* implemented. <br>
*
*/
  public void test_ListIterator(){
    th.checkPoint("listIterator()java.util.ListIterator");
    LinkedList ll = new LinkedList();
    ListIterator li = ll.listIterator();
    try {
     	li.next();
     	th.fail("should throw a NoSuchElementException -- 1");
    }
    catch(NoSuchElementException nsee) { th.check(true, "caught exeption -- 1"); }
    try {
     	li.previous();
     	th.fail("should throw a NoSuchElementException -- 2");
    }
    catch(NoSuchElementException nsee) { th.check(true, "caught exeption -- 2"); }
    th.check(!li.hasNext() , "no elements ... -- 1");
    th.check(!li.hasPrevious() , "no elements ... -- 1");
    th.check(li.nextIndex() , 0 ,"nextIndex == 0 -- 1");
    th.check(li.previousIndex() , -1 ,"previousIndex == -1 -- 1");
    li.add("a");
    th.check(!li.hasNext() , "no elements ... -- 2");
    th.check(li.hasPrevious() , "one element ... -- 2");
    th.check(li.nextIndex() , 1 ,"nextIndex == 1 -- 2");
    th.check(li.previousIndex() , 0 ,"previousIndex == 0 -- 2");
    try {
     	li.next();
     	th.fail("should throw a NoSuchElementException -- 3");
    }
    catch(NoSuchElementException nsee) { th.check(true, "caught exeption -- 3"); }
    th.check("a".equals(li.previous()) , "checking previous element -- 1");
    li.add(null);
    th.check(ll.getFirst() == null , "checking if LinkedList got updated -- 1");
    th.check("a",ll.getLast() ,"checking if LinkedList got updated -- 2");
//    th.debug(ll.toString());
    th.check(li.previousIndex() , 0 ,"previousIndex == 0 -- 3");
    th.check(li.previous() == null , "checking previous element -- 2");
    th.check(li.next() == null , "checking next element -- 1");
    li.add("b");
    th.check("a".equals(li.next()) ,"checking next element -- 2");
    li.add("c");
    try {
     	li.set("not");
     	th.fail("should throw a IllegalStateException -- 1");
    }
    catch(IllegalStateException ise) { th.check(true, "caught exeption -- 4"); }
    th.check(!ll.contains("not"), "set should not have been executed");
    try {
     	li.remove();
     	th.fail("should throw a IllegalStateException -- 2");
    }
    catch(IllegalStateException ise) { th.check(true, "caught exeption -- 5"); }
    catch(Exception e) { th.fail("wrong exception was thrown"); }
    th.check("c".equals(li.previous()) , "checking previous element -- 3");
    li.set("new");
    th.check("new".equals(li.next()) , "validating set");
    li.set("not");
    li.set("notOK");
    li.remove();
    try {
     	li.set("not");
     	th.fail("should throw a IllegalStateException -- 3");
    }
    catch(IllegalStateException ise) { th.check(true, "caught exeption -- 6"); }
    th.check(!ll.contains("not"), "set should not have been executed");
    try {
     	li.remove();
     	th.fail("should throw a IllegalStateException -- 4");
    }
    catch(IllegalStateException ise) { th.check(true, "caught exeption -- 7"); }
    catch(Exception e) { th.fail("wrong exception was thrown"); }
    try {
     	li.next();
     	th.fail("should throw a NoSuchElementException -- 4");
    }
    catch(NoSuchElementException nsee) { th.check(true, "caught exeption -- 8"); }
    th.check("a",li.previous(),"checking on previous element");
    li.remove();
    try {
     	li.set("not");
     	th.fail("should throw a IllegalStateException -- 5");
    }
    catch(IllegalStateException ise) { th.check(true, "caught exeption -- 9"); }
    th.check(!ll.contains("not"), "set should not have been executed");
    try {
     	li.remove();
     	th.fail("should throw a IllegalStateException -- 6");
    }
    catch(IllegalStateException ise) { th.check(true, "caught exeption -- 10"); }
    catch(Exception e) { th.fail("wrong exception was thrown"); }

  }

}
