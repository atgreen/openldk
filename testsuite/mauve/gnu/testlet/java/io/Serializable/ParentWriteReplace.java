// Tags: JDK1.1

// Copyright (C) 2005 Daniel Bonniot <bonniot@users.sf.net>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.io.Serializable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;

/**
 * Check when a parent writeReplace() method should be used for subclasses.
 *
 * Here is the rationale for this test, based on Sun's javadoc for
 * java.io.Serializable:
 *
 * First, writeReplace must be declared with:
 *   ANY-ACCESS-MODIFIER Object writeReplace() throws ObjectStreamException;
 * Thus, a non-private writeReplace is OK.
 *
 * Second, the rules for invoking writeReplace are that
 * it must be called if it would be accessible from the class of the object
 * being deserialized. So, a non-private writeReplace method in the parent must
 * be called, but a private one should not be.
 */
public class ParentWriteReplace implements Testlet {

  public void test(TestHarness harness)
  {
    try {
      ByteArrayOutputStream outb = new ByteArrayOutputStream();
      ObjectOutputStream outs = new ObjectOutputStream(outb);

      MySingleton singleton = new MySingleton();
      outs.writeObject(singleton);
      harness.check(singleton.replaced);

      Foo foo = new MyFoo();
      outs.writeObject(foo);
      harness.check(! foo.replaced);
    }
    catch (Throwable e) {
      harness.debug(e);
    }
  }

  //// Singleton/MySingleton with a non-private writeReplace ////

  static abstract class Singleton implements java.io.Serializable
  {
    boolean replaced = false;

    /** NOTE: this writeReplace is not private.
     */
    Object writeReplace() {
      replaced = true;
      return this;
    }
  }

  static class MySingleton extends Singleton
  {
  }

  //// Foo/MyFoo with a private writeReplace ////

  static abstract class Foo implements java.io.Serializable
  {
    boolean replaced = false;

    /** NOTE: this writeReplace is private.
     */
    private Object writeReplace() {
      replaced = true;
      return this;
    }
  }

  static class MyFoo extends Foo
  {
  }
}
