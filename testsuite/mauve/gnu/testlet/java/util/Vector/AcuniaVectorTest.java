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

package gnu.testlet.java.util.Vector;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.*;
import java.lang.NullPointerException;
import java.lang.UnsupportedOperationException;

/**
* This file contains testcode for java.util.Vector.<br>	
* <br>
* WRITTEN BY ACUNIA <br>
*  <br>
*
*/
public class AcuniaVectorTest extends Vector implements Testlet
{
  protected TestHarness th;

  public AcuniaVectorTest() { /* Public Constructor needed for Testlet */ }

  public void test (TestHarness harness)
    {
       th = harness;
       test_Vector();
       test_contains();
       test_containsAll();
       test_indexOf ();
       test_isEmpty();
       test_lastIndexOf();
       test_size();
       test_get ();
       test_copyInto();
       test_elementAt();
       test_elements();
       test_firstElement();
       test_lastElement();
       test_add();
       test_addAll();
       test_addElement();
       test_clear();
       test_insertElementAt();
       test_remove();
       try { test_removeAll(); }
       catch (UnsupportedOperationException ue) {th.fail("method removeAll() is not supported");}
       test_removeAllElements();
       test_removeElement();
       test_removeElementAt();
       test_removeRange();
       try {test_retainAll();}
       catch (UnsupportedOperationException ue) {th.fail("method retainAll() is not supported");}
       try { test_set(); }
       catch (UnsupportedOperationException ue) {th.fail("method set() is not supported");}
       test_setElementAt();
       test_setSize();
       test_capacity();
       test_ensureCapacity();
       test_trimToSize();
       try { test_subList(); }
       catch (UnsupportedOperationException ue) {th.fail("method subList() is not supported");}
       try { test_toArray(); }
       catch (ArrayStoreException ae) { th.fail("failure in System.arraycopy, got exception: "+ae); }
       test_clone();
       test_equals();
       test_hashCode();
       test_toString();
       test_behaviour();
       test_iterator();
     }
  public Vector buildknownV() {

    	Vector v = new  Vector();
    	Float f;    	
  	for (int i =0; i < 11; i++)
        {
        f = new Float((float)i);
        v.addElement(f);
        }
        return v;

  }

/**
* implemented.
*
*/
  public void test_Vector(){
    th.checkPoint("Vector()");
    Vector v = new Vector();
    th.check(v.capacity()==10 , "check default capacity");
    th.checkPoint("Vector(java.util.Collection)");
    v = new Vector(buildknownV());
    v.equals(buildknownV());
    th.checkPoint("Vector(int)");
    v = new Vector(20);
    th.check(v.capacity()==20 , "check default capacity");
    th.checkPoint("Vector(int,int)");
    v = new Vector(20,5);
    th.check(v.capacity()==20 , "check default capacity");

  }

/**
* implemented.
*
*/
  public void test_contains(){
    th.checkPoint("contains(java.lang.Object)boolean");
    Vector v = buildknownV();
    Object o = new Object();
    Float f = new Float(5.0f);
    th.check(!v.contains(null) , "null is allowed -- 1");
    v.addElement(null);
    th.check(v.contains(null) ,  "null is allowed -- 2");
    th.check(  v.contains(f) , "contains -- 1");
    f = new Float(15.0f);
    th.check(! v.contains(f) , "contains -- 2");
    th.check(! v.contains(o) , "contains -- 3");
    v.addElement(o);
    th.check(  v.contains(o) , "contains -- 4");
    th.check(! v.contains(new Object()) , "contains -- 5");

  }

/**
*   implemented.<br>
*   since jdk 1.2  <br>
*   needs extra testing <br>
*/
  public void test_containsAll(){
    th.checkPoint("containsAll(java.util.Colection)boolean");
    Vector v = new Vector();
    try { v.containsAll(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }
    v.addElement("a");  v.addElement("b");  v.addElement("c");
    v.addElement(null);
    Collection c = (Collection) v.clone();
    v.addElement("d");  v.addElement("e");  v.addElement("f");
    th.check(v.containsAll(c) , "checking ContainsAll -- 1");
    v.removeElement("a");
    th.check(!v.containsAll(c) , "checking ContainsAll -- 2");

    try { v.containsAll(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }
	
  }

/**
* implemented.
*
*/
  public void test_indexOf(){
    th.checkPoint("indexOf(java.lang.Object)int");
    Vector v = buildknownV();
    Object o = new Object();
    Float f = new Float(5.0f);
    th.check( v.indexOf(f) == 5 , "contains -- 1");
    f = new Float(15.0f);
    th.check( v.indexOf(f) == -1 , "contains -- 2");
    th.check( v.indexOf(o) == -1, "contains -- 3");
    v.addElement(o);
    th.check( v.indexOf(o) == 11 , "contains -- 4");
    th.check( v.indexOf(new Object()) == -1 , "contains -- 5");
    try  {v.indexOf(null);
          th.check(true);
          v.addElement(null);
          th.check(v.indexOf(null) == 12, "null was added to the Vector");
         }
    catch(NullPointerException ne) { th.fail("shouldn't throw NullPointerException"); }

    th.checkPoint("indexOf(java.lang.Object,int)int");
    v = buildknownV();
    o = new Object();
    f = new Float(5.0f);
    th.check( v.indexOf(f,2) == 5 , "contains -- 1");
    th.check( v.indexOf(f,6) == -1 , "contains -- 2");
    f = new Float(15.0f);
    th.check( v.indexOf(f,4) == -1 , "contains -- 3");
    th.check( v.indexOf(o,3) == -1, "contains -- 4");
    v.addElement(o);
    th.check( v.indexOf(o,11) == 11 , "contains -- 5");
    v.addElement(f);
    th.check( v.indexOf(o,12) == -1 , "contains -- 6");

    th.check( v.indexOf(new Object(),1) == -1 , "contains -- 7");
    try  {v.indexOf(null,3);
          th.check(true);
          v.addElement(null);
          th.check(v.indexOf(null,13) == 13, "null was added to the Vector");
         }
    catch(NullPointerException ne) { th.fail("shouldn't throw NullPointerException"); }
    try  {f = new Float(10.0f);
          th.check(v.indexOf(f,333)== -1 ,"checking bounderies");

         }
    catch(Exception ne) { th.fail("shouldn't throw an Exception"); }
    try  {v.indexOf(f,-1);
          th.fail("shouldn't throw NullPointerException");
         }
    catch(Exception ne) { th.check(true); }

  }

/**
* implemented.
*
*/
  public void test_isEmpty(){
    th.checkPoint("isEmpty()boolean");
    Vector v = new Vector();
    th.check(v.isEmpty() ,"testing isEmpty -- 1");
    th.check(!buildknownV().isEmpty() ,"testing isEmpty -- 2");

  }

/**
* implemented .
*
*/
  public void test_lastIndexOf(){
    th.checkPoint("lastIndexOf(java.lang.Object)int");
    Vector v = buildknownV();
    Object o = new Object();
    Float f = new Float(5.0f);
    th.check( v.lastIndexOf(f) == 5 , "contains -- 1");
    f = new Float(15.0f);
    th.check( v.lastIndexOf(f) == -1 , "contains -- 2");
    th.check( v.lastIndexOf(o) == -1, "contains -- 3");
    v.addElement(o);
    th.check( v.lastIndexOf(o) == 11 , "contains -- 4");
    th.check( v.lastIndexOf(new Object()) == -1 , "contains -- 5");
    try  {v.lastIndexOf(null);
          th.check(true);
          v.addElement(null);
          th.check(v.lastIndexOf(null) == 12, "null was added to the Vector");
         }
    catch(NullPointerException ne) { th.fail("shouldn't throw NullPointerException"); }

    th.checkPoint("lastIndexOf(java.lang.Object,int)int");
    v = buildknownV();
    o = new Object();
    f = new Float(5.0f);
    th.check( v.lastIndexOf(f,5) == 5 , "contains -- 1");
    th.check( v.lastIndexOf(f,4) == -1 , "contains -- 2");
    f = new Float(15.0f);
    th.check( v.lastIndexOf(f,4) == -1 , "contains -- 3");
    th.check( v.lastIndexOf(o,3) == -1, "contains -- 4");
    v.addElement(o);
    th.check( v.lastIndexOf(o,11) == 11 , "contains -- 5");
    v.addElement(f);
    th.check( v.lastIndexOf(o,10) == -1 , "contains -- 6");

    th.check( v.lastIndexOf(new Object(),10) == -1 , "contains -- 7");
    try  {v.lastIndexOf(null,12);
          th.check(true);
          v.addElement(null);
          th.check(v.lastIndexOf(null,13) == 13, "null was added to the Vector");
          th.check(v.lastIndexOf(null,12) == -1, "null was added to the Vector, on pos 13");

         }
    catch(NullPointerException ne) { th.fail("shouldn't throw NullPointerException"); }
    try  {f = new Float(10.0f);
          th.check(v.lastIndexOf(f,-1)== -1 ,"checking bounderies");

         }
    catch(Exception ne) { th.fail("shouldn't throw an Exception"); }
    try  {v.lastIndexOf(f,91);
          th.fail("shouldn't throw NullPointerException");
         }
    catch(Exception ne) { th.check(true); }

  }

/**
* implemented .
*
*/
  public void test_size(){
    th.checkPoint("size()int");
    Vector v = buildknownV();
    th.check(v.size() == 11 , "size -- 1 - got: "+v.size());
    v.addElement(null);
    th.check(v.size() == 12 , "size -- 2 - got: "+v.size());
    v.addElement(new Object());
    th.check(v.size() == 13 , "size -- 3 - got: "+v.size());
    v = new Vector();
    th.check(v.size() == 0 ,  "size -- 4 - got: "+v.size());

  }

/**
*   implemented.<br>	
*   since jdk 1.2
*/
  public void test_get(){
    th.checkPoint("get(int)java.lang.Object");
    Vector v = buildknownV();
    try { v.get(-1);
          th.fail("should throw exception -- 1");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try { v.get(11);
          th.fail("should throw exception -- 2");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    v = new Vector();
    v.addElement("a");  v.addElement(null);  v.addElement("c");
    v.addElement(null);
    th.check(v.get(0).equals("a") && v.get(2).equals("c") , "checking get -- 1");
    th.check(v.get(1) == null && v.get(3) == null , "checking get -- 2");
  }

/**
* implemented.
*
*/
  public void test_copyInto(){
    th.checkPoint("copyInto([java.lang.Object)void");
    Vector v = buildknownV();
    StringBuffer bf= new StringBuffer("smartmove");
    v.addElement(bf);
    Object o[] = new Object[5];
    try { v.copyInto(o);
    	  th.fail("should throw ArrayIndexOutOfBoundsException");
        }
    catch(ArrayIndexOutOfBoundsException ae) { th.check(true); }
    o = new Object[15];
    v.copyInto(o);
    for (int i=0 ; i < 11 ; i++ )
    { th.check( o[i] == v.elementAt(i),"checking copyInto -- "+(i+1)+" - got: "+o[i]); }
    th.check( o[11] == v.elementAt(11) , "checking stringbuffer");
    th.check(o.length == 15);
  }

/**
* implemented.
*
*/
  public void test_elementAt(){
    th.checkPoint("elementAt(int)java.lang.Object");
    Vector v = new Vector();
    v.addElement(null);
    Float f =new Float(23.0f);
    Double d =new Double(54.5);
    v.addElement(f);
    v.addElement(d);
    th.check( v.elementAt(0) == null );
    th.check( v.elementAt(1) == f );
    th.check( v.elementAt(2) == d );
    try  {v.elementAt(-1);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 1");
    	  }	
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try  {v.elementAt(3);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 2");
    	  }	
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    	

  }

/**
* implemented.
*
*/
  public void test_elements(){
    th.checkPoint("elements()java.util.Enumeration");
    Vector v = buildknownV();
    v.addElement(null);
    Float f =new Float(23.0f);
    Double d =new Double(54.5);
    v.addElement(f);
    v.addElement(d);
    Enumeration e = v.elements();
    for (int i=0; i < 11 ; i++ )
    { th.check( ((Float)e.nextElement()).intValue() == i,"checking elements -- "+(i+1)); }
    try {
    th.check(e.hasMoreElements(), "null in vector might give problems -- 1");
    th.check(e.nextElement() == null ,  "null in vector might give problems -- 2");
    th.check(e.hasMoreElements(), "null in vector might give problems -- 3");
    th.check(e.nextElement() == f ,  "null in vector might give problems -- 4");
    th.check(e.nextElement() == d ,  "null in vector might give problems -- 5");
    th.check(!e.hasMoreElements(), "null in vector might give problems -- 6");
    }
    catch (Exception te) { th.debug("caught unwanted exception: "+te); }
    v = new Vector();
    th.check( v.elements() != null);
  }

/**
* implemented.
*
*/
  public void test_firstElement(){
    th.checkPoint("firstElement()java.lang.Object");
    Vector v = new Vector();
    try { v.firstElement();
    	  th.fail("should throw NoSuchElementException");
        }
    catch(NoSuchElementException ne) { th.check(true); }

    v.addElement(null);
    th.check(v.firstElement() == null );
    v = new Vector();
    Float f =new Float(23.0f);
    v.addElement(f);
    th.check(v.firstElement() == f );
    v = new Vector();
    Double d =new Double(54.5);
    v.addElement(d);
    v.addElement(null);
    v.addElement(f);
    th.check(v.firstElement() == d );

  }

/**
* implemented.
*
*/
  public void test_lastElement(){
    th.checkPoint("lastElement()java.lang.Object");
    Vector v = new Vector();
    try { v.lastElement();
    	  th.fail("should throw NoSuchElementException");
        }
    catch(NoSuchElementException ne) { th.check(true); }

    v.addElement(null);
    th.check(v.lastElement() == null );
    v = buildknownV();
    v.addElement(null);
    th.check(v.lastElement() == null );
    Float f =new Float(23.0f);
    v.addElement(f);
    th.check(v.lastElement() == f );
    v = new Vector();
    Double d =new Double(54.5);
    v.addElement(d);
    th.check(v.lastElement() == d );

  }

/**
*   implemented. <br>
*   since jdk 1.2
*/
  public void test_add(){
    th.checkPoint("add(java.lang.Object)boolean");
    Vector v = new Vector();
    th.check(v.add("a") && v.add(null), "checking returns boolean -- 1");
    th.check(v.add("c") && v.add(null), "checking returns boolean -- 2");
    th.check(v.get(0)=="a" && v.get(2)=="c", "checking addedcat right position -- 1");
    th.checkPoint("add(int,java.lang.Object)void");
    v = buildknownV();
    try { v.add(-1,"a");
          th.fail("should throw exception -- 1");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try { v.add(12,"a");
          th.fail("should throw exception -- 2");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try { v.add(11,"a");
          th.check(true);
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.fail("shouldn't throw exception -- 1"); }
    v = new Vector();
    v.add(0,"a");  v.add(0,null);
    v.add(1,"c");  v.add(2,null);
    th.check(v.get(3).equals("a") && v.get(1).equals("c") , "checking get -- 1");
    th.check(v.get(0) == null && v.get(2) == null , "checking get -- 2");
    v.add(4,"b");  v.add(5,null);
    th.check(v.get(4) == "b" && v.get(5) == null , "checking get -- 3");

  }

/**
*   implemented.<br>
*   since jdk 1.2
*/
  public void test_addAll(){
    th.checkPoint("addAll(java.util.Collection)boolean");
    Vector v =new Vector();
    try { v.addAll(null);
          th.fail("should throw NullPointerException");
        }
    catch (NullPointerException ne) { th.check(true); }
    Collection c = (Collection) v;
    th.check(!v.addAll(c) ,"checking returnvalue -- 1");
    v.add("a"); v.add("b"); v.add("c");
    c = (Collection) v;
    v = buildknownV();
    th.check(v.addAll(c) ,"checking returnvalue -- 2");
    th.check(v.containsAll(c), "extra on containsAll -- 1");
    th.check(v.get(11)=="a" && v.get(12)=="b" && v.get(13)=="c", "checking added on right positions");

    th.checkPoint("addAll(int,java.util.Collection)boolean");
    v =new Vector();
    c = (Collection) v;
    th.check(!v.addAll(0,c) ,"checking returnvalue -- 1");
    v.add("a"); v.add("b"); v.add("c");
    c = (Collection) v;
    v = buildknownV();
    try { v.addAll(-1,c);
          th.fail("should throw exception -- 1");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try { v.addAll(12,c);
          th.fail("should throw exception -- 2");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try { th.check(v.addAll(11,c),"checking returnvalue -- 2"); }
    catch (ArrayIndexOutOfBoundsException ae) { th.fail("shouldn't throw exception -- 1"); }
    th.check(v.containsAll(c), "extra on containsAll -- 1");
    th.check(v.get(11)=="a" && v.get(12)=="b" && v.get(13)=="c", "checking added on right positions -- 1");
    th.check(v.addAll(1,c),"checking returnvalue -- 3");
    th.check(v.get(1)=="a" && v.get(2)=="b" && v.get(3)=="c", "checking added on right positions -- 2");
  }

/**
* implemented.  <br>
* just very, very basic testing <br>
* --> errors in addElement will also make others tests fail
*/
  public void test_addElement(){
    th.checkPoint("addElement(java.lang.Object)void");
    Vector v = new Vector();
    v.addElement("a");
    th.check(v.size() == 1 , "check size -- 1");
    th.check(v.elementAt(0) == "a" );
    v.addElement(null);
    th.check(v.size() == 2 , "check size -- 2");
    th.check(v.elementAt(1) == null );

  }

/**
*   implemented. <br>
*   since jdk 1.2
*/
  public void test_clear(){
    th.checkPoint("clear()void");
    Vector v = buildknownV();
    int c = v.capacity();
    v.clear();
    th.check(v.isEmpty() && (v.size()==0), "make sure all is gone");
    th.check(c == v.capacity() , "capacity stays the same, got: "+v.capacity()+", but exp.: "+c);

  }

/**
* implemented.
*
*/
  public void test_insertElementAt(){
    th.checkPoint("insertElementAt(java.lang.Object,int)void");
    Vector v = buildknownV();
    v.insertElementAt("a",5);
    int i;
    for (i=0 ; i < 5 ; i++ )
    { th.check( ((Float)v.elementAt(i)).intValue() ==  i, "Float value didn't change -- "+(i+1)); }
    for (i=6 ; i < 12 ; i++ )
    { th.check( ((Float)v.elementAt(i)).intValue() ==  i-1 , "checking shifted elements -- "+(i-5)); }
    th.check(v.elementAt(5) == "a" );
    try {
    	v.insertElementAt("a",12);
	th.check(v.elementAt(12) == "a" );
        }
    catch (Exception e) { th.fail("shouldn't throw an Exception -- caught: "+e); }
    try {
    	v.insertElementAt("a",14);
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 1" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    try {
    	v.insertElementAt("a",-1);
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 2" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    v = buildknownV();
    v.insertElementAt(null,5);
    for (i=0 ; i < 5 ; i++ )
    { th.check( ((Float)v.elementAt(i)).intValue() ==  i, "Float value didn't change inserted null -- "+(i+1)); }
    for (i=6 ; i < 12 ; i++ )
    { th.check( ((Float)v.elementAt(i)).intValue() ==  i-1 , "checking shifted elements inserted null -- "+(i-5)); }
    th.check(v.elementAt(5) == null );

  }

/**
*   implemented. <br>
*   since jdk 1.2
*/
  public void test_remove(){
  th.checkPoint("remove(int)java.lang.Object");
    Vector v = buildknownV();
    try {
    	v.remove(-1);
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 1" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    try {
    	v.remove(11);
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 2" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    th.check( ((Float)v.remove(5)).intValue() == 5 , "checking returnvalue remove -- 1");
    int i;  boolean ok = true;
    th.check(v.size() == 10 , "checking new Size");
    for (i=0; i < 5 ; i++)
    { if (((Float)v.get(i)).intValue() != i) ok = false; }
    th.check(ok , "checking order Floats in Vector -- 1");   	
    ok = true;
    for (i=5; i < 10 ; i++)
    { if (((Float)v.get(i)).intValue() != (i+1)) ok = false; }
    th.check(ok , "checking order Floats in Vector -- 2");   	
    v.add(5,null);
    th.check( v.remove(5) == null , "checking returnvalue remove -- 2");
    ok = true;
    for (i=0; i < 5 ; i++)
    { if (((Float)v.get(i)).intValue() != i) ok = false; }
    th.check(ok , "checking order Floats in Vector -- 3");   	
    ok = true;
    for (i=5; i < 10 ; i++)
    { if (((Float)v.get(i)).intValue() != (i+1)) ok = false; }
    th.check(ok , "checking order Floats in Vector -- 4");   	
    th.check( ((Float)v.remove(9)).intValue() == 10 , "checking returnvalue remove -- 3");
    th.check( ((Float)v.remove(0)).intValue() == 0 , "checking returnvalue remove -- 4");
    th.check( ((Float)v.remove(0)).intValue() == 1 , "checking returnvalue remove -- 5");
    v = new Vector();
    try {
    	v.remove(0);
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 3" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
 th.checkPoint("remove(java.lang.Object)boolean");
    v = new Vector();
    th.check(!v.remove("a") ,"checking remove on empty vector-- 1");
    th.check(!v.remove("a") ,"checking remove on empty vector-- 2");
    v.add("a"); v.add(null); v.add("a");
    v.add("c"); v.add("d"); v.add(null);
    th.check(v.remove("a") ,"checking returnvalue remove -- 1");
    th.check(v.get(0)==null && v.get(1)=="a" && v.get(2)=="c" &&v.get(3)=="d" &&v.get(4)==null , "checking order of elements -- 1");
    th.check(v.remove("a") ,"checking returnvalue remove -- 2");
    th.check(v.get(0)==null && v.get(1)=="c" &&v.get(2)=="d" &&v.get(3)==null , "checking order of elements -- 2");
    th.check(!v.remove("a") ,"checking returnvalue remove -- 3");
    th.check(v.get(0)==null && v.get(1)=="c" &&v.get(2)=="d" &&v.get(3)==null , "checking order of elements -- 3");
    th.check(v.remove(null) ,"checking returnvalue remove -- 4");
    th.check(v.get(0)=="c" &&v.get(1)=="d" &&v.get(2)==null , "checking order of elements -- 4");
    th.check(v.remove(null) ,"checking returnvalue remove -- 5");
    th.check(v.get(0)=="c" &&v.get(1)=="d", "checking order of elements -- 5");
    th.check(!v.remove(null) ,"checking returnvalue remove -- 6");
    th.check(v.get(0)=="c" &&v.get(1)=="d", "checking order of elements -- 6");


  }

/**
*   implemented. <br>
*   since jdk 1.2
*/
  public void test_removeAll() throws UnsupportedOperationException{
    th.checkPoint("removeAll(java.util.Collection)boolean");
    Vector v = new Vector();

  /**
   * Disabled. A little too strict.
     We will allow removing null from an empty Vector.
    try { v.removeAll(null);
          th.fail("should throw NullPointerException");
    }
    catch (NullPointerException ne) { th.check(true); }
   */

    v.add("a");
    try { v.removeAll(null);
          th.fail("should throw NullPointerException");
    }
    catch (NullPointerException ne) { th.check(true); }
    th.debug(v.toString());
    v.add("b");
    th.debug(v.toString());
    v.add(null); 
    th.debug(v.toString() + " == c");
    Collection c = (Collection) v;
    v = buildknownV();
    th.debug(v.toString() + " == v");
    th.check(!v.removeAll(c) , "checking returnvalue of removeAll -- 1");
    th.debug(v.toString());
    th.check(v.equals(buildknownV()) , "v didn't change");
    th.debug(c.toString() + " == c");
    v.addAll(c);
    th.debug(v.toString() + " v afet addAll(c)");
    th.check(v.removeAll(c) , "checking returnvalue of removeAll -- 2");
    th.debug(v.toString() + " v after removeAll(c)");
    th.check(v.equals(buildknownV()) , "v did change to original v");
    v.add(2,null);
    th.debug(v.toString() + "add(2,null)");
    v.add(4,"a");
    th.debug(v.toString() + "add(4,a)");
    v.add(9,"b");
    th.debug(v.toString() + "add(9,b)");
    v.addAll(0,c);
    th.debug(v.toString());
    v.add(2,null); v.add(4,"a"); v.add(9,"b");   v.addAll(c);
    th.debug(v.toString());
    th.check(v.removeAll(c) , "checking returnvalue of removeAll -- 3");
    th.debug(v.toString());
    th.check(v, buildknownV(), "make sure all elements are removed");
    th.debug(v.toString());
  }

/**
* implemented.
*
*/
  public void test_removeAllElements(){
    th.checkPoint("removeAllElements()void");
    Vector v = buildknownV();
    int c = v.capacity();
    v.removeAllElements();
    th.check(v.isEmpty() && (v.size()==0), "make sure all is gone");
    th.check(c == v.capacity() , "capacity stays the same, got: "+v.capacity()+", but exp.: "+c);

  }

/**
* implemented.
*
*/
  public void test_removeElement(){
    th.checkPoint("removeElement(java.lang.Object)boolean");
    Vector v = buildknownV();
    v.addElement("a");     v.addElement(null);
    v.addElement("a");     v.addElement(null);
    v.addElement("a");     v.addElement(null);
    th.check(v.removeElement("a") , "element is in there -- 1");
    th.check(v.size() == 16 , "size is one less -- 1");
    th.check(!v.removeElement("c") , "element isn't in there -- 1");

    int i;  boolean ok = true;
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 1");
    th.check( (v.elementAt(11) == null) && (v.elementAt(13) == null) && (v.elementAt(15) == null) ,"checking order -- 1");
    th.check( (v.elementAt(12) == "a") && (v.elementAt(14) == "a") ,"checking order -- 2");
    ok = true;
    th.check(v.removeElement("a") , "element is in there -- 2");
    th.check(v.size() == 15 , "size is one less -- 2");
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 2");
    th.check( (v.elementAt(11) == null) && (v.elementAt(12) == null) && (v.elementAt(14) == null) ,"checking order -- 3");
    th.check( (v.elementAt(13) == "a") ,"checking order -- 4");
    ok = true;
    th.check(v.removeElement("a") , "element is in there -- 3");
    th.check(v.size() == 14 , "size is one less -- 3");
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 3 ");
    th.check( (v.elementAt(11) == null) && (v.elementAt(12) == null) && (v.elementAt(13) == null) ,"checking order -- 5");
    th.check(!(v.contains("a")) ,"checking contents -- 1");
    th.check(!v.removeElement("a") , "element isn't in there -- 2");
    ok = true;
    th.check(v.removeElement(null) , "element is in there -- 4");
    th.check(v.size() == 13 , "size is one less -- 4");
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 4");
    th.check( (v.elementAt(11) == null) && (v.elementAt(12) == null) ,"checking order -- 6");
    th.check(v.removeElement(null) , "element is in there -- 5");
    th.check(v.removeElement(null) , "element is in there -- 6");
    th.check(!v.removeElement(null) , "element isn't in there -- 3");


  }

/**
* implemented.
*
*/
  public void test_removeElementAt(){
    th.checkPoint("removeElementAt(int)void");
    Vector v = buildknownV();
    v.addElement("a");     v.addElement("b");
    v.addElement("a");     v.addElement(null);
    v.addElement("a");     v.addElement("b");
    v.removeElementAt(11);
    th.check(v.size() == 16 , "size is one less -- 1");

    int i;  boolean ok = true;
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 1");
    th.check( (v.elementAt(11) == "b") && (v.elementAt(13) == null) && (v.elementAt(15) == "b") ,"checking order -- 1");
    th.check( (v.elementAt(12) == "a") && (v.elementAt(14) == "a") ,"checking order -- 2");

    ok = true;
    v.removeElementAt(12);
    th.check(v.size() == 15 , "size is one less -- 2");
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 2");
    th.check( (v.elementAt(11) == "b") && (v.elementAt(12) == null) && (v.elementAt(14) == "b") ,"checking order -- 3");
    th.check( (v.elementAt(13) == "a") ,"checking order -- 4");

    ok = true;
    v.removeElementAt(13);
    th.check(v.size() == 14 , "size is one less -- 3");
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 3 ");
    th.check( (v.elementAt(11) == "b") && (v.elementAt(12) == null) && (v.elementAt(13) == "b") ,"checking order -- 5");
    th.check(!(v.contains("a")) ,"checking contents -- 1");
    ok = true;
    v.removeElementAt(12);
    th.check(v.size() == 13 , "size is one less -- 4");
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 4");
    th.check( (v.elementAt(11) == "b") && (v.elementAt(12) == "b") ,"checking order -- 5");
    th.check(!(v.contains(null)) ,"checking contents -- 2");
    v = buildknownV();
    try { v.removeElementAt(-1);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 1");
    	}
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try { v.removeElementAt(11);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 2");
    	}
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
  }

/**
*   implemented.<br>
*   since jdk 1.2 <br>
*   removeRange is a protected method
*/
  public void test_removeRange(){
    th.checkPoint("removeRange(int,int)void");
    AcuniaVectorTest xal = new AcuniaVectorTest(buildAL());
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
    catch(IllegalArgumentException iae) {
	th.fail("should throw an IndexOutOfBoundsException, "
		+ "not IllegalArumentException -- 3");
    }
    try {
    	th.check(al.equals(xal) , "ArrayList must not be changed -- 3");
    }
    catch(Exception e) { th.debug("bad operations messed up the Vector");}
    xal = new AcuniaVectorTest(buildAL());
    xal.ensureCapacity(40);
    try {
    	xal.removeRange(3,15);
        th.fail("should throw an IndexOutOfBoundsException -- 4");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    th.check(xal.equals(al) , "ArrayList must not be changed -- 4");
    xal = new AcuniaVectorTest(buildAL());
    xal.ensureCapacity(40);
    try {
    	xal.removeRange(15,13);
        th.fail("should throw an IndexOutOfBoundsException -- 5");
        }
    catch(IndexOutOfBoundsException ioobe) { th.check(true); }
    catch(IllegalArgumentException iae) {
	th.fail("should throw an IndexOutOfBoundsException, "
		+ "not IllegalArumentException -- 5");
    }
    try {
    	th.check(xal.equals(al) , "ArrayList must not be changed -- 5");
    }
    catch(Exception e) { th.debug("bad operations messed up the Vector");}
    xal = new AcuniaVectorTest(buildAL());
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
*   implemented.<br>
*   since jdk 1.2
*/
  public void test_retainAll(){
    th.checkPoint("retainAll(java.util.Collection)boolean");
    Vector v = new Vector();
  /**
   * Disabled. A little too strict.
     We will allow retaining null from an empty Vector.
    try { v.retainAll(null);
          th.fail("should throw NullPointerException");
    }
    catch (NullPointerException ne) { th.check(true); }
   */
    v.add("a");
    try { v.retainAll(null);
          th.fail("should throw NullPointerException");
    }
    catch (NullPointerException ne) { th.check(true); }
    v.add("b"); v.add(null);
    Collection c = (Collection) v;
    v = buildknownV();
    th.check(v.retainAll(c) , "checking returnvalue of retainAll -- 1");
    th.check(v.size() == 0 , "v is emptied");
    v = buildknownV();
    v.addAll(c);
    th.check(v.retainAll(c) , "checking returnvalue of retainAll -- 2");
    th.check(v.get(2)==null && v.get(1)=="b" && v.get(0)=="a" , "v is has elements of c");
    th.check(v.equals(c) , "extra check on Vector.equals()");
    th.check(v.size() == 3 , "checking new size() -- 1");
    v = buildknownV();
    th.debug(v + " == v");
    v.add(2,null);
    th.debug(v + " - add(2,null)");
    v.add(4,"a");
    th.debug(v + " - add(4,a)");
    v.add(9,"b");
    th.debug(v + " - add(9,b)");
    th.debug(c + " == c");
    v.addAll(10,c);
    th.debug(v + " - add(10,c)");
    boolean b = v.retainAll(c);
    th.debug(v + " - retainAll(c)");
    th.check(b , "checking returnvalue of retainAll -- 3");
    th.check(v.get(0)==null && v.get(2)=="b" && v.get(1)=="a" , "multiple copies of an element shouldn't be deleted -- 1"+v);
    th.check(v.get(5)==null && v.get(4)=="b" && v.get(3)=="a" , "multiple copies of an element shouldn't be deleted -- 2"+v);
    th.check(v.size() == 6 , "checking new size() -- 2");
    v = buildknownV();
    v.add(2,null);
    th.check(v.retainAll(c) , "checking returnvalue of retainAll -- 3");
    th.check(v.get(0)==null , "checking contents of the vector -- 1");
    th.check(v.size() == 1 , "checking new size() -- 2");

  }

/**
*   implemented.<br>
*   since jdk 1.2
*/
  public void test_set() throws UnsupportedOperationException{
    th.checkPoint("set(int,java.lang.Object)java.lang.Object");
    Vector v = new Vector();
    try {
    	v.set(-1,"a");
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 1" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    try {
    	v.set(0,"a");
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 2" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    v = buildknownV();
    try {
    	v.set(-1,"a");
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 3" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    try {
    	v.set(11,"a");
	th.fail("should throw an ArrayIndexOutOfBoundsException -- 4" );
        }
    catch (ArrayIndexOutOfBoundsException e) { th.check(true); }
    th.check( ((Float)v.set(5,"a")).intValue()==5 , "checking returnvalue of set -- 1");
    th.check( ((Float)v.set(0,null)).intValue()==0 , "checking returnvalue of set -- 2");
    th.check( v.get(5) == "a" , "checking effect of set -- 1");
    th.check( v.get(0) == null , "checking effect of set -- 2");
    th.check( v.set(5,"a") == "a" , "checking returnvalue of set -- 3");
    th.check( v.set(0,null) == null , "checking returnvalue of set -- 4");
    th.check( v.get(5) == "a" , "checking effect of set -- 1");
    th.check( v.get(0) == null , "checking effect of set -- 2");
  }

/**
* implemented.
*
*/
  public void test_setElementAt(){
    th.checkPoint("setElementAt(java.lang.Object,int)void");
    Vector v = buildknownV();
    try { v.setElementAt("a",-1);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 1");
    	}
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    try { v.setElementAt("a",11);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 2");
    	}
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }

    v.setElementAt("a",5);
    th.check(v.elementAt(5) == "a" , "validate set -- 1");
    v.setElementAt("b",0);
    th.check(v.elementAt(0) == "b" , "validate set -- 2");
    v.setElementAt("c",10);
    th.check(v.elementAt(10) == "c" , "validate set -- 3");
    v.setElementAt("d",5);
    th.check(v.elementAt(5) == "d" , "validate set -- 4");
    th.check(!v.contains("a"), "check contents -- 1");
    v.setElementAt(null,5);
    th.check(v.elementAt(5) == null , "validate set -- 5");
    v.setElementAt("a",5);
    th.check(v.elementAt(5) == "a" , "validate set -- 6");
    th.check(!v.contains(null), "check contents -- 2");


  }

/**
* implemented.
*
*/
  public void test_setSize(){
    th.checkPoint("setSize(int)void");
    Vector v = buildknownV();

    try { v.setSize(-1);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 1");
    	}
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true, "good job!"); }
    int i;  boolean ok = true;
    int size = 25;
    v.setSize(size);
    th.check(v.size() == size ,"checking new size -- 1");

    try { v.elementAt(size);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 2");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 1");
    ok = true;
    for (i=11 ; i < size ; i++ )
    { if (v.elementAt(i)!=  null) ok = false; }
    th.check( ok , "null value not added -- 1");
    size =5;
    v.setSize(size);
    th.check(v.size() == size ,"checking new size -- 2");
    for (i=0 ; i < size ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float value didn't change -- 2");
    try { v.elementAt(size);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 3");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }
    size =0;
    v.setSize(size);
    th.check(v.size() == size ,"checking new size -- 3");
    try { v.elementAt(size);
    	  th.fail("should throw ArrayIndexOutOfBoundsException -- 4");
        }
    catch (ArrayIndexOutOfBoundsException ae) { th.check(true); }

  }

/**
* implemented.<br>
* --> errors in capacity will make ensurecapacity fail
*/
  public void test_capacity(){
    th.checkPoint("capacity()int");
    Vector v = new Vector(15);
    th.check(v.capacity() == 15, "checking capacity");
  }

/**
* implemented.
*
*/
  public void test_ensureCapacity(){
    th.checkPoint("ensureCapacity(int)void");
    Vector v = new Vector(10);
    v.ensureCapacity(9);
    th.check(v.capacity() == 10, "checking capacity -- 1");
    v.ensureCapacity(15);
    th.check(v.capacity() == 20, "checking capacity -- 2");
    v.ensureCapacity(41);
    th.check(v.capacity() == 41, "checking capacity -- 3");
    v = new Vector(10,15);
    v.ensureCapacity(9);
    th.check(v.capacity() == 10, "checking capacity -- 4");
    v.ensureCapacity(15);
    th.check(v.capacity() == 25, "checking capacity -- 5");
    v.ensureCapacity(55);
    th.check(v.capacity() == 55, "checking capacity -- 6");

  }

/**
* implemented.
*
*/
  public void test_trimToSize(){
    th.checkPoint("trimToSize()void");
    Vector v = buildknownV();
    int size = v.size();
    v.ensureCapacity(20);
    v.trimToSize();
    th.check( v.capacity() == size );
    int i;  boolean ok = true;
    for (i=0 ; i < 11 ; i++ )
    { if (((Float)v.elementAt(i)).intValue() !=  i) ok = false; }
    th.check( ok , "Float values didn't change -- 1");
    v.addElement("a");
    th.check(v.capacity() == 22, "adding an elements raises the capacity");
  }

/**
*   implemented  --> MIGHT NEED EXTRA TESTING. <br>
*   since jdk 1.2  <br>
*   the behaviour of the subList related to the  Vector is not tested <br>
*   completly --> may be tested in other places?
*/
  public void test_subList() throws UnsupportedOperationException{
    th.checkPoint("subList(int,int)java.util.List");
    Vector v = new Vector();
    try {
    	v.subList(-1,0);
	th.fail("should throw an IndexOutOfBoundsException -- 1" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    try {
    	v.subList(0,1);
	th.fail("should throw an IndexOutOfBoundsException -- 2" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    try {
	th.check(v.subList(0,0).size()==0);
        }
    catch (IndexOutOfBoundsException e) { th.fail("shouldn't throw an IndexOutOfBoundsException -- 3" ); }
    try {
    	v.subList(1,0);
	th.fail("should throw an IllegalArgumentException -- 4" );
        }
    catch (IllegalArgumentException e) { th.check(true); }
    v = buildknownV();
    try {
    	v.subList(-1,6);
	th.fail("should throw an IndexOutOfBoundsException -- 5" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    try {
    	v.subList(10,9);
	th.fail("should throw an IllegalArgumentException -- 6" );
        }
    catch (IllegalArgumentException e) { th.check(true); }
    try {
    	v.subList(1,12);
	th.fail("should throw an IndexOutOfBoundsException -- 7" );
        }
    catch (IndexOutOfBoundsException e) { th.check(true); }
    try {
	th.check(v.subList(11,11).size() ==0 );
        }
    catch (IndexOutOfBoundsException e) { th.fail("shouldn't throw an IndexOutOfBoundsException -- 8" ); }
    List l = v.subList(0,11);
    th.check(v.equals(l) , "checking sublist for equality");
    v.add("a");
    try { l.get(3);
	th.fail("should throw a ConcurrentModificationException -- 1" );
        }
    catch (ConcurrentModificationException e) { th.check(true); }
    v = new Vector();
    v.add("a"); v.add("b"); v.add(null); v.add("c"); v.add("d");
    l = v.subList(2,5);
    th.check(l.get(0) == null && l.get(1)=="c" && l.get(2)=="d" , "checking elements -- 1");
    th.check(l.set(0,"g")==null , "checking set");
    th.check(v.get(2)=="g" , "modifications in l should reflect on v -- 1");
    th.check(l.get(0) == "g" && l.get(1)=="c" && l.get(2)=="d" , "checking elements -- 1");
    l.clear();
    th.check(v.size()==2 && v.get(0)=="a" && v.get(1)=="b" ,"modifications in l should reflect on v -- 1");
    //th.debug("DEBUG -- done with check");
    v.add(null); v.add("c"); v.add("d");
    //th.debug("DEBUG -- done adding");
    l = v.subList(2,5);
    //th.debug("DEBUG -- done sublisting");

    try { v.addAll(l);
          th.fail("should throw a ConcurrentModificationException");
          // during this method call l might be overridden ...
        }
    catch (ConcurrentModificationException e) { th.check(true); }
  }

/**
*   implemented.<br>
*   since jdk 1.2
*/
  public void test_toArray(){
  th.checkPoint("toArray()[java.lang.Object");
    Vector v = new Vector();
    v.add("a"); v.add(null); v.add("b");
    Object o[]=v.toArray();
    th.check(o[0]== "a" && o[1] == null && o[2] == "b" , "checking elements -- 1");
    th.check(o.length == 3 , "checking size Object array");

  th.checkPoint("toArray([java.lang.Object)[java.lang.Object");
    v = new Vector();
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
    v = buildknownV();
    try { v.toArray(null);
          th.fail("should throw NullPointerException -- 2");
        }
    catch (NullPointerException ne) { th.check(true); }
    try { v.toArray(sa);
          th.fail("should throw an ArrayStoreException");
        }
    catch (ArrayStoreException ae) { th.check(true); }
    v.add(null);
    Float far[],fa[] = new Float[12];
    far = (Float[])v.toArray(fa);
    th.check( far == fa , "returned array is the same");
    try { sa = (String[])v.toArray(fa);
          th.fail("should throw ClassCastException");
        }
    catch (ClassCastException ce) { th.check(true); }

  }

/**
* implemented.
*
*/
  public void test_clone(){
    th.checkPoint("clone()java.lang.Object");
    Vector cv,v = new Vector(10,5);
    v.addElement("a")	;v.addElement("b")    ;v.addElement("c");
    cv = (Vector)v.clone();
    th.check(cv.size() == v.size(), "checking size -- 1");
    th.check(cv.capacity() , v.capacity(), "checking capacity -- 1");
    cv.ensureCapacity(11);
    th.check(cv.capacity() , 15, "capacityIncrement was not defined correctly");
    //this could fail if capacity is not cloned correctly ...
    th.check(v.capacity() == 10, "changes in one object doen't affect the other -- 1");
    v.addElement("d");
    th.check(cv.size() == 3, "changes in one object doen't affect the other -- 2");


  }

/**
* implementation. <br>
* overrides Object.equals() since jdk1.2 <br>
*
*/
  public void test_equals(){
    th.checkPoint("equals(java.lang.Object)boolean");
    Vector v = buildknownV();
    th.check(v.equals(buildknownV()), "objects are equal -- 1");
    v.removeElementAt(1);
    th.check(!v.equals(buildknownV()), "objects are not equal -- 1");
    v = buildknownV();
    v.ensureCapacity(25);
    th.check(v.equals(buildknownV()), "objects are equal -- 2");
    ArrayList al = new ArrayList(v);
    th.check(v.equals(al) , "checking ... -- 1");
    v = new Vector();
    al = new ArrayList();
    th.check(v.equals(al) , "checking ... -- 2");
    v.add(null);
    al.add(null);
    th.check(v.equals(al) , "checking ... -- 3");
    v.add("a"); v.add(null);
    al.add(null); al.add("a");
    th.check(!v.equals(al) , "checking ... -- 4");
  }

/**
* implemented.
*
*/
  public void test_hashCode(){
    th.checkPoint("hashCode()int");
    Vector v = new Vector();
    th.check(v.hashCode() == 1 , "check calculation hashcode -- 1 - got: "+v.hashCode());
    v.addElement("a");
    th.check(v.hashCode() == 31 + "a".hashCode() , "check calculation hashcode -- 2 - got: "+v.hashCode());
    Integer i = new Integer(324);
    v.addElement(i);
    th.check(v.hashCode() == 31*31 + 31*"a".hashCode() + i.hashCode() , "check calculation hashcode -- 3 - got: "+v.hashCode());
    v = new Vector();
    v.addElement(null);
    th.check(v.hashCode() == 31, "check calculation hashcode -- 4 - got: "+v.hashCode());

  }

/**
* implemented.
*
*/
  public void test_toString(){
    th.checkPoint("toString()java.lang.String");
    Vector v =new Vector();
    th.check(v.toString().equals("[]"), "checking toString -- 1 - got: "+v);
    v.addElement("a");
    th.check(v.toString().equals("[a]"), "checking toString -- 2 - got: "+v);
    Integer i = new Integer(324);
    v.addElement(i);
    th.check(v.toString().equals("[a, 324]"), "checking toString -- 3 - got: "+v);
    v.addElement("abcd");
    th.check(v.toString().equals("[a, 324, abcd]"), "checking toString -- 4 - got: "+v);


  }
/**
* implemented. <br>
* <br>
* this method isn't defined in Vector but we test the inherited method ...
*/
  public void test_iterator(){
    th.checkPoint("iterator()java.util.Iterator");
    Vector v = new Vector();
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");
    v.add(null);    v.add("!");
    Iterator it = v.iterator();	
    Vector vc = (Vector) v.clone();
    int i=0;
    Object o;
    while (it.hasNext()) {
    	o = it.next();
    	if (!vc.remove(o)) th.debug("didn't find "+o+" in vector");	
    	if (i++> 20)  break;
    }
    th.check( i < 20 , "check for infinite loop");
    th.check(vc.isEmpty() ,"all elements iterated");
    try {
    	it.next();
    	th.fail("should throw a NoSuchElementException");
    	}
    catch(NoSuchElementException nsee) { th.check(true); }
    it = v.iterator();	
    try {
    	it.remove();
    	th.fail("should throw an IllegalStateException -- 1");
    	}
    catch(IllegalStateException ise) { th.check(true); }
    it.next();
    it.remove();
    try {
    	it.remove();
    	th.fail("should throw an IllegalStateException -- 2");
    	}
    catch(IllegalStateException ise) { th.check(true); }
    v.add("new");
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- 1");
    	}
    catch(ConcurrentModificationException cme) { th.check(true); }
    try {
    	it.remove();
    	th.fail("should throw a ConcurrentModificationException -- 2");
    	}
    catch(ConcurrentModificationException cme) { th.check(true); }
    catch(IllegalStateException ise) { th.debug("ConcurrentModificationException should be triggered first"); }

    it = v.iterator();	
    while (it.hasNext()) {
    	o = it.next();
    	it.remove();
    	if (v.contains(o)) th.fail("removed wrong element when tried to remove "+o+", got:"+v); 	
    	if (i++> 20)  break;
    }
    th.check(v.isEmpty() , "all elements are removed");
// check if modCount is updated correctly !!!
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");
    it = v.iterator();	
    v.contains("a");
    v.containsAll(v);
    v.indexOf("a");
    v.isEmpty();
    v.lastIndexOf("a");
    v.size();
    v.get(2);
    v.hashCode();
    v.equals(v);
    v.toArray();
    v.toArray(new Object[2]);
    v.copyInto(new Object[10]);
    v.elementAt(2);
    v.elements();
    v.firstElement();
    v.lastElement();	
    v.capacity();
    v.trimToSize();
    v.subList(2,5);
    v.clone();
    v.toString();
    try {
    	it.next();
    	th.check(true, "Ok -- 1");
    	}
    catch(Exception e) { th.fail("should not throw an Exception, got "+e);}
    it = v.iterator();	
    v.add(3,"a");
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- add(int,Object)");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 2"); }
    it = v.iterator();	
    v.add("a");
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- add(Object)");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 3"); }
    it = v.iterator();	
    v.addAll((Collection)v.clone());
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- addAll(int,Col)");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 4"); }
    it = v.iterator();	
    v.addAll(3,(Collection)v.clone());
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- addAll(Col)");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 5"); }
    it = v.iterator();	
    v.addElement("b");
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- addElement");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 6"); }
    it = v.iterator();	
    v.insertElementAt("b",4);
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- insertElementAt");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 7"); }
    it = v.iterator();	
    v.remove(4);
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- remove(int)");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 8"); }
    it = v.iterator();	
    v.remove("b");
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- remove(Object)");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 9"); }
    Vector rv = new Vector();
    rv.add("a"); rv.add("b");
    it = v.iterator();	
    v.removeAll(rv);
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- removeAll");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 10"); }
    it = v.iterator();	
    v.removeElement("c");
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- removeElement");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 11"); }
    it = v.iterator();	
    v.removeElementAt(7);
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- removeElementAt");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 12"); }
    rv = (Vector)v.clone();
    rv.remove(null);      rv.remove(null);        rv.remove(null);
    v.add(null);
    it = v.iterator();	
    v.retainAll(rv);
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- retainAll");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 13"); }
    it = v.iterator();	
    v.setSize(v.size()-2);
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- setSize");
    	}
    catch(ConcurrentModificationException cme) { th.check(true, "Ok -- 16"); }
    it = v.iterator();	
    v.removeAllElements();
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- removeAllElements");
    	}
    catch(ConcurrentModificationException cme) { th.check(true); }
    catch(NoSuchElementException nsee) { th.debug("ConcurrentModificationException should be triggered first"); }
    v.add("a");
    it = v.iterator();	
    v.clear();
    try {
    	it.next();
    	th.fail("should throw a ConcurrentModificationException -- clear");
    	}
    catch(ConcurrentModificationException cme) { th.check(true); }
    catch(NoSuchElementException nsee) { th.debug("ConcurrentModificationException should be triggered first"); }

  }


/**
*   not implemented. <br>
*   excessive testing
*/
  public void test_behaviour(){
    th.checkPoint("()");
    Vector v = buildknownV();
    ArrayList al = new ArrayList(v);
    th.check( al.size() == v.size(), "checking size");
    Iterator it = al.iterator();
    it.next();
    try {
    	it.remove();
    	th.check(true , "passed remove");	
        }
    catch(Exception e) { th.fail("got bad exception, "+e); }

  }

  protected ArrayList buildAL() {
    Vector v = new Vector();
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    v.add("a");     v.add("c");   v.add("u");   v.add("n");   v.add("i");   v.add("a");  v.add(null);
    return new ArrayList(v);

  }

// The following fields and method are necessary for extending and Vector.

  private boolean didRemoveRange=false;
  private int from = -1;
  private int to   = -1;

  public AcuniaVectorTest(Collection c){
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
