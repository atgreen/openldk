/* Copyright (C) 2001, 2004 ACUNIA

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

package gnu.testlet.java.util.Properties;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.*;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;

/**
*  Written by ACUNIA.<br>
*  <br>
*  this file contains test for java.util.Properties <br>
*  <br>
*  Properties extends Hashtable and thus all methods !!! <br>
*  We can use methods from Hashtable to get info about the Properties object <br>
*  <br>
*  Whenever a stream is needed we choose an ByteArrayStream  <br>
*/
public class AcuniaPropertiesTest implements Testlet
{
  protected TestHarness th;
  protected Properties defProps;
  protected byte buffer[];
  protected byte bytesout[];
  protected ByteArrayInputStream bin;
  protected ByteArrayOutputStream bout;
  protected PrintStream psout;
  protected PrintWriter pwout;
  public void test (TestHarness harness)
    {
       th = harness;

       setUpTest();
       resetStreams();

       test_Properties();
       resetStreams();
       test_getProperty();
       resetStreams();
       test_list();
       resetStreams();
       test_load();
       resetStreams();
       test_propertyNames();
       resetStreams();
       test_setProperty();
       resetStreams();
       test_store();
       resetStreams();
       test_save();
       resetStreams();
       test_loadextra();
     }

  private void resetStreams(){
  	try {
  	bin.reset();
  	bout.reset();
  	}
  	catch (Exception e){}
  }

  private void setUpTest() {
   	buffer = new String("name=yes\nSmart=move\nanimal=dog").getBytes();
        bin = new ByteArrayInputStream(buffer);
        bout = new ByteArrayOutputStream();
        psout = new PrintStream(bout);
        pwout = new PrintWriter(bout);
   	defProps = new Properties();
   	try { defProps.load(bin); }
   	catch (Exception e) {}
   	buffer =new String("!comment\n   \nname=no\n#morecomments\ndog=no_cat\ntest\ndate=today\nlongvalue=I'mtryingtogiveavaluelongerthen40characters\n40chars=thisvalueshouldcontainexactly40charcters").getBytes();
        bin = new ByteArrayInputStream(buffer);   	
  }

/**
* implemented.	<br>
* might need extra testcode
*/
  public void test_Properties(){
   th.checkPoint("Properties()");
    // not much to check for this one !
    Properties p = new Properties();
    th.check(p.isEmpty() ,"nothing in there");
   th.checkPoint("Properties(java.util.Properties)");
    p = new Properties(defProps);
    th.check(p.isEmpty() ,"nothing in there");
    th.check(p.getProperty("name").equals("yes") , "default field is not empty");
    try {  p = new Properties(null);
           th.check(true);
        }
    catch (Exception e) { th.fail("should not throw an Exeption. Got: "+e); }
  }

/**
* implemented.
*
*/
  public void test_getProperty(){
    th.checkPoint("getProperty(java.lang.String)java.lang.String");
    Properties p = new Properties();
    try { p.getProperty(null);
          th.fail("should throw a NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check( true ); }

    p = new Properties(defProps);
    try { p.getProperty(null);
          th.fail("should throw a NullPointerException -- 2");
        }
    catch (NullPointerException ne) { th.check( true ); }
    try { p.load(bin); }
    catch (Exception e) {}
    try { p.getProperty(null);
          th.fail("should throw a NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check( true ); }
    try {
    th.check(p.getProperty("dog").equals("no_cat") , "check returnvalue");
    th.check(p.getProperty("name").equals("no") , "return property from main property table");
    th.check(p.getProperty("Smart").equals("move") , "check returnvalue from default table");
    th.check(p.getProperty("NoEntry") == null , "check for null if not there");
    }
    catch (Exception e) { th.fail("got unexpected exception: "+e); }
    th.checkPoint("getProperty(java.lang.String,java.lang.String)java.lang.String");
    try { p.getProperty(null, "Noooo...");
          th.fail("should throw a NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check( true ); }
    try { th.check(p.getProperty( "Noooo...", null)==null , "defVal may be null");   }
    catch (NullPointerException ne) { th.fail("shouldn't throw a NullPointerException -- 1"); }
    th.check(p.getProperty("dog","not found").equals("no_cat") , "check returnvalue");
    th.check(p.getProperty("name","not found").equals("no") , "return property from main property table");
    th.check(p.getProperty("Smart","not found").equals("move") , "check returnvalue from default table");
    th.check(p.getProperty("NoEntry","not found").equals("not found") , "check for defVal if not there");

  }

/**
* implemented.
*
*/
  public void test_list(){
   th.checkPoint("list(java.io.PrintStream)void");
    Properties p = new Properties();
    try { p.list((PrintStream)null);
        th.fail("should throw NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check(true); }

    try { p.load(bin); }
    catch (Exception e) {}
    p.list(psout);
    byte ba[] = bout.toByteArray();
    Vector v = new Vector();
    Enumeration ek = p.keys();
    String s;
    while (ek.hasMoreElements())
    { s = (String) ek.nextElement();
      v.add(s+"="+p.getProperty(s));
    }
    v.add("Smart=move"); v.add("animal=dog");

    int start,count =0;
    v.removeElement("longvalue=I'mtryingtogiveavaluelongerthen40characters");
    v.add("longvalue=I'mtryingtogiveavaluelongerthen40char...");
    while ( count < ba.length ) {
    start = count;
    while ( ba[count] !='\n' && count < ba.length) { count++;}
    s = new String(ba , start , count - start);
    if (!s.startsWith("--")) // list() adds a header
	th.check(v.contains(s), "v does not contain:$"+s+"$");
    v.removeElement(s);
    count++;
    }

    try { p.list((PrintStream)null);
        th.fail("should throw NullPointerException -- 2");
        }
    catch (NullPointerException ne) { th.check(true); }

   th.checkPoint("list(java.io.PrintWriter)void");
    resetStreams();
    p = new Properties();
    try { p.list((PrintWriter)null);
        th.fail("should throw NullPointerException -- 1");
        }
    catch (NullPointerException ne) { th.check(true); }

    try { p.load(bin); }
    catch (Exception e) {}
    p.list(pwout);
    ba = bout.toByteArray();
    v = new Vector();
    ek = p.keys();
    while (ek.hasMoreElements())
    { s = (String) ek.nextElement();
      v.add(s+"="+p.getProperty(s));
    }
    v.add("Smart=move"); v.add("animal=dog");

    count =0;
    v.removeElement("longvalue=I'mtryingtogiveavaluelongerthen40characters");
    v.add("longvalue=I'mtryingtogiveavaluelongerthen40char...");

    while ( count < ba.length ) {
    start = count;
    while ( ba[count] !='\n' && count < ba.length) { count++;}
    s = new String(ba , start , count - start);
    if (!s.startsWith("--")) // list() adds a header
      th.check(v.contains(s), "v does not contain:$"+s+"$");
    v.removeElement(s);
    count++;
    }

    try { p.list((PrintStream)null);
        th.fail("should throw NullPointerException -- 2");
        }
    catch (NullPointerException ne) { th.check(true); }
  }

/**
* implemented.	<br>
* load is used by other tests to make a propeties file <br>
* failures in load will mak other tests fail !
*/
  public void test_load(){
    th.checkPoint("load(java.io.InputStream)void");
    Properties p = new Properties();
    try  { p.load((ByteArrayInputStream) null);
    	   th.fail("should throw NullPointerException");
         }
    catch (NullPointerException ne) { th.check(true); }
    catch (Exception e) {th.fail("should throw an NullPointerException instead of: "+e); }

    try { p.load(bin); }
    catch (Exception e) {}
    Enumeration ek1 = p.keys();
    resetStreams();
    try { p.load(bin); }
    catch (Exception e) {}
    Enumeration ek2 = p.keys();
    boolean ok = true;
    while (ek1.hasMoreElements() && ek2.hasMoreElements())
    { if (ek1.nextElement() != ek2.nextElement()) ok = false; }
    th.check( !ek1.hasMoreElements() && !ek2.hasMoreElements(), "no extra elements may be added with same name");
    th.check( ok , " all elements are equal ");
    bin = new ByteArrayInputStream(new String("name=yes\nSmart=move\nanimal=dog").getBytes());
    try { p.load(bin); }
    catch (Exception e) {}
    th.check(p.getProperty("name").equals("yes") , "load overrides previous values");
    Vector v = new Vector();
    v.add("name"); v.add("Smart"); v.add("animal");
    v.add("dog"); v.add("test"); v.add("date"); v.add("longvalue"); v.add("40chars");
    ek1 = p.keys();  ok = true ; Object o;
    while (ek1.hasMoreElements())
    {   o = ek1.nextElement();
    	if ( v.contains(o)) v.removeElement(o);
    	else { ok = false; th.debug("got extra element: "+(String)o);}
    }

    th.check( ok , "all elements were there" );
    th.check( v.isEmpty() , "all elements should be gone, got"+v );
    setUpTest();


  }

/**
* implemented.
*
*/
  public void test_propertyNames(){
    th.checkPoint("propertyNames()java.util.Enumeration");
    Properties p = new Properties();
    try { p.load(bin); }
    catch (Exception e) {}

    Enumeration en = p.propertyNames();
    Enumeration ek = p.keys();
    boolean ok = true;
    Vector v = new Vector();
    Enumeration ek2 = p.keys();
    while (ek2.hasMoreElements()) {
      v.add(ek2.nextElement ());
    }
    while (ek.hasMoreElements() && en.hasMoreElements())
    {
     ek.nextElement();
     Object next = en.nextElement();
     if (!v.contains(next))
     {
       ok = false;
       th.debug(next + " not in " + v);
     }
    }
    th.check(ok , "all elements are the same");
    th.check( ! ek.hasMoreElements() &&  ! en.hasMoreElements() , "make sure both enumerations are empty");
    p = new Properties(defProps);
    resetStreams();
    try { p.load(bin); }
    catch (Exception e) {}
    v.add("Smart"); v.add("animal");
    en = p.propertyNames();
    ok = true;
    Object o;
    while (en.hasMoreElements()){
      o = en.nextElement();
      if ( v.contains(o)) v.removeElement(o);
      else { ok = false;
             th.debug("got extra element: "+o);
           }
    }
    th.check(ok , "see if no double were generated");
    th.check(v.isEmpty() , "check if all names were mentioned -- got:"+v);
  }

/**
* implemented.
*
*/
  public void test_setProperty(){
    th.checkPoint("setProperty(java.lang.String,java.lang.String)java.lang.Object");
    Properties p = new Properties();
    try {
    	p.setProperty(null ,"Noooo...");
        th.fail("should throw NullPointerException -- 1");
        }
     catch (NullPointerException ne) { th.check(true); }
    try {
    	p.setProperty("Noooo...", null);
        th.fail("should throw NullPointerException -- 2");
        }
     catch (NullPointerException ne) { th.check(true); }
     p = new Properties(defProps);
     try { p.load(bin); }
     catch (Exception e) {}
     try {
    	 p.setProperty(null ,"Noooo...");
         th.fail("should throw NullPointerException -- 3");
         }
     catch (NullPointerException ne) { th.check(true); }
     try {
     	 p.setProperty("No again...", null);
         th.fail("should throw NullPointerException -- 4");
         }
     catch (NullPointerException ne) { th.check(true); }
     try {
     th.check(((String)p.setProperty("test","null")).equals("") , "returns \"\" in our case");
     }
     catch (NullPointerException ne) { th.fail("the value a of property cannot be null, got:"+ne); }
     th.check(p.getProperty("test").equals("null") , "check new value in our case null");
     th.check(p.setProperty("testing","null") == null , "returns value null, name not in list");
     th.check(p.getProperty("test").equals("null") , "check new value in our case null");
     String s = (String)p.setProperty("Smart","nul");
     th.check(s == null , "returnvalue, is null default list not touched, got");
     th.check(p.getProperty("Smart").equals("nul") , "check new value in our case null");
     s = ((String)p.setProperty("name","nu"));
     th.check(s.equals("no") , "return value in our case no, got: "+s);
     th.check(p.getProperty("name").equals("nu") , "check new value in our case nu");


  }

/**
* implemented.	<br>
* this method replaces save.  It behaves the same, but it will throw a <br>
* java.io.IOException if an IO error occurs, while save did nothing <br>
* <br>
* to test this we should create an OutputStream wich will be guaranteed to fail ! <br>
* ???? must be added !!!! (how) <br>
* Add a test to determine of store generates a comment line with the current time <br>
*/
  public void test_store(){
    th.checkPoint("store(java.io.OutputStream,java.lang.String)void");
    Properties p = new Properties(defProps);
    try  { p.store((ByteArrayOutputStream) null , "no comment");
    	   th.fail("should throw NullPointerException");
         }
    catch (NullPointerException ne) { th.check(true); }
    catch (Exception e) {th.fail("should throw an NullPointerEception instead of: "+e); }

    try  { p.store(bout , null);
    	   th.check(true);
         }
    catch (NullPointerException ne) { th.fail("should not throw NullPointerException"); }
    catch (Exception e) {th.fail("shouldn't throw any Exception, but got: "+e); }

    resetStreams();
    try {p.store(bout, null); }
    catch (Exception e) {th.fail("shouldn't throw any Exception, but got: "+e); }
    byte ba[] = bout.toByteArray();
    th.check( (ba[0]== (byte) '#') && (ba[1]!= (byte) '#'), "only the date should be written");
    th.check(ba.length < 50 , "default properties are never printed out");
    resetStreams();
    try { p.load(bin); }
    catch (Exception e) {}
    try {p.store(bout, "no comments");}
    catch (Exception e) {th.fail("shouldn't throw any Exception, but got: "+e); }
    ba = bout.toByteArray();
    String s = new String(ba , 0 , 12 );
    th.check( s.equals("#no comments"), "got: "+s);
    int i = 0 , count = 0;
    while ( i < 2 && count < ba.length)
    { if (ba[count++] == (byte) '\n') i++ ;}
// we will construct a vector containing all the lines with should be written
    Vector v = new Vector();
    Enumeration ek = p.keys();
    while (ek.hasMoreElements())
    { s = (String) ek.nextElement();
      v.add(s+"="+p.getProperty(s));
    }
    while ( count < ba.length ) {
    int start = count;
    while (count < ba.length) {
    if ( ba[count] !='\n'  ) count++;
    else break;
    }
    s = new String(ba , start , count - start);
    th.check(v.contains(s), "v does not contain: "+s);
    v.removeElement(s);
    count++;
    }
  }

/**
* implemented.	<br>
* Add a test to determine of store generates a comment line with the current time <br>
*  <br>
*  depricated method !!!
*/
  public void test_save(){
    th.checkPoint("save(java.io.OutputStream,java.lang.String)void");
    Properties p = new Properties(defProps);
    try  { p.save(null , "no comment");
    	   th.fail("should throw NullPointerException");
         }
    catch (NullPointerException ne) { th.check(true); }
    catch (Exception e) {th.fail("should throw an NullPointerEception instead of: "+e); }

    try  { p.save(bout , null);
    	   th.check(true);
         }
    catch (NullPointerException ne) { th.fail("should not throw NullPointerException"); }
    catch (Exception e) {th.fail("shouldn't throw any Exception, but got: "+e); }

    resetStreams();
    try {p.save(bout, null); }
    catch (Exception e) {th.fail("shouldn't throw any Exception, but got: "+e); }
    byte ba[] = bout.toByteArray();
    th.check( (ba[0]== (byte) '#') && (ba[1]!= (byte) '#'), "just date should be written");
    th.debug(ba.length + " -- got: " +new String(ba));
    th.check(ba.length < 50 , "default properties are never printed out");
    resetStreams();
    try { p.load(bin); }
    catch (Exception e) {}
    try {p.save(bout, "no comments");}
    catch (Exception e) {th.fail("shouldn't throw any Exception, but got: "+e); }
    ba = bout.toByteArray();
    String s = new String(ba , 0 , 12 );
    th.check( s.equals("#no comments"), "got: "+s);
    int i = 0 , count = 0;
    while ( i < 2 && count < ba.length)
    { if (ba[count++] == (byte) '\n') i++ ;}
// we will construct a vector containing all the lines with should be written
    Vector v = new Vector();
    Enumeration ek = p.keys();
    while (ek.hasMoreElements())
    { s = (String) ek.nextElement();
      v.add(s+"="+p.getProperty(s));
    }
    while ( count < ba.length ) {
    int start = count;
    while (count < ba.length) {
    if ( ba[count] !='\n') count++;
    else break;
    }
    s = new String(ba , start , count - start);
    th.check(v.contains(s), "v does not contain: "+s);
    v.removeElement(s);
    count++;
    }

  }
/**
* this test will check if all forms lines are accepted in the load.
*
*/
  public void test_loadextra(){
    th.checkPoint("load(java.io.InputStream)void");
    Properties p = new Properties();
    buffer =new String("   !comment\n \t  \nname = no\r    #morec\tomm\\\nents\r\n  dog=no\\\\cat   \nburps    :\ntest=\ndate today\n\n\nlong\\\n   value=tryin \\\n gto\n4:vier\nvier     :4").getBytes();
    bin = new ByteArrayInputStream(buffer);   	
    try {p.load(bin);} catch (Exception e) {}
    Enumeration e = p.keys();
    Vector v = new Vector();
    // Note that there used to be code here checking whether the
    // "!comment" and "#morec" keys were found, on the theory that
    // leading whitespace mattered.  This no longer seems to be the
    // case, however.  In the past it apparently varied between JVMs,
    // but the 1.4 docs are unambiguous on this topic.  We check for
    // "ents" since line-continuation doesn't affect comments.
    v.add("ents=");
    v.add("name=no");
    v.add("dog=no\\cat   ");
    v.add("burps=");
    v.add("test=");
    v.add("date=today");
    v.add("longvalue=tryin gto");
    v.add("4=vier");
    v.add("vier=4");
    String s;
    while ( e.hasMoreElements()) {
         s = (String) e.nextElement();
         th.debug("checkvalue -- got:$"+s+"="+p.getProperty(s)+"$");
         th.check(v.contains(s+"="+p.getProperty(s)), "checkvalue -- got:$"+s+"="+p.getProperty(s)+"$");
         v.removeElement(s+"="+p.getProperty(s));
   }
   th.check( v.isEmpty() , "check if all elements were found -- got: "+v);
 }
}

