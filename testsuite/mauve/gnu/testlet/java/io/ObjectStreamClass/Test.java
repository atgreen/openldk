// Tags: JDK1.1
// Uses: A B C Defined DefinedNotFinal DefinedNotStatic NotSerial Serial

/* Test.java -- Tests ObjectStreamClass class

   Copyright (c) 1998, 2002 by Free Software Foundation, Inc.
   Written by Geoff Berry <gcb@gnu.org>.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation
   Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA */

package gnu.testlet.java.io.ObjectStreamClass;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.ObjectStreamClass;
import java.util.Vector;

public class Test implements Testlet
{
  public void testLookup (Class cl, boolean is_null)
  {
    ObjectStreamClass osc = ObjectStreamClass.lookup (cl);
    harness.check (is_null ? osc == null : osc != null);
  }

  public void testGetName (Class cl, String name)
  {
    harness.check (ObjectStreamClass.lookup (cl).getName (), name);
  }

  public void testToString (Class cl, String str, long uid)
  {
    String s = ObjectStreamClass.lookup (cl).toString (); 
    harness.check (s.indexOf(str) != -1
		   || s.indexOf(Long.toString(uid)) != -1,
		   "Should contain '" + str + "' or '" + uid + "'");
  }

 public void testForClass (Class cl, Class clazz)
  {
    harness.check (ObjectStreamClass.lookup (cl).forClass (), clazz);
  }

  public void testSUID (Class cl, long suid)
  {
    harness.check (ObjectStreamClass.lookup (cl).getSerialVersionUID (), suid);
  }

  public void test (TestHarness harness)
  {
    this.harness = harness;

    // lookup
    harness.checkPoint ("lookup");
    testLookup (Serial.class, false);
    testLookup (NotSerial.class, true);

    // getName
    harness.checkPoint ("getName");
    testGetName (java.lang.String.class, "java.lang.String");
    testGetName (java.util.Hashtable.class, "java.util.Hashtable");

    // toString 
    harness.checkPoint ("toString");
    testToString (java.lang.String.class,
		  "java.lang.String", -6849794470754667710L);

    // forClass
    harness.checkPoint ("forClass");
    testForClass (java.lang.String.class, java.lang.String.class);
    testForClass (java.util.Vector.class, (new Vector ()).getClass ());

    // getSerialVersionUID
    harness.checkPoint ("getSerialVersionUID");
    testSUID (A.class, -4758524860474883287L);
    testSUID (B.class, -5709768504584827290L);

    // NOTE: this fails for JDK 1.1.5v5 on linux because a non-null
    // jmethodID is returned from
    // GetStaticMethodID (env, C, "<clinit>", "()V")
    // even though class C does not have a class initializer.
    // The JDK's serialver tool does not have this problem somehow.
    // I have not tested this on other platforms.
    testSUID (C.class, 7295418696978364872L);

    testSUID (Defined.class, 17);
    testSUID (DefinedNotStatic.class, -4424277244062554359L);
    testSUID (DefinedNotFinal.class, -1176535035944461302L);
    testSUID (A[].class, 3317986791243421446L);
  }

  TestHarness harness;
}
