// Tags: JDK1.1

// Copyright (C) 2006 Free Software Foundation, Inc.
// Written by Mark J. Wielaard  (mark@klomp.org)

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.


package gnu.testlet.java.lang.ClassLoader;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class loadClass extends ClassLoader implements Testlet
{
  public void test(TestHarness harness)
  {
    ClassLoader cl = this.getClass().getClassLoader();
    boolean cnf_thrown;
    try
      {
	cl.loadClass("gnu");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("gnu.");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass(".");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("/");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[[");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[]");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("L;");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("L.");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("L[");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[L;");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[L[;");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[L.;");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[Lgnu;");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("[Lgnu.;");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);

    try
      {
	cl.loadClass("");
	cnf_thrown = false;
      }
    catch(ClassNotFoundException x)
      {
	cnf_thrown = true;
      }
    harness.check(cnf_thrown);
  }
}
