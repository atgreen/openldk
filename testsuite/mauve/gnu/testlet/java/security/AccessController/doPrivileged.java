// Copyright (C) 2005, Free Software Foundation, Inc.
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA, 02110-1301 USA.
//
// Tags: JDK1.2

package gnu.testlet.java.security.AccessController;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.*;

/**
 * Checks that unchecked exceptions are properly thrown and checked
 * exceptions are properly wrapped.
 *
 * Written by Mark J. Wielaard. Suggested by Nicolas Geoffray.
 */
public class doPrivileged implements Testlet, PrivilegedExceptionAction
{

  // The thing to throw.
  private Throwable t;

  public void test(TestHarness harness)
  {
    doPrivileged pea = new doPrivileged();

    pea.t = new NullPointerException();
    try
      {
	AccessController.doPrivileged(pea);
      }
    catch (NullPointerException npe)
      {
	harness.check(true);
      }
    catch (Throwable tt)
      {
	harness.debug(tt);
	harness.check(false);
      }

    pea.t = new java.io.IOException();
    try
      {
	AccessController.doPrivileged(pea);
      }
    catch (PrivilegedActionException pae)
      {
	harness.check(pea.t, pae.getCause());
      }
    catch (Throwable tt)
      {
	harness.debug(tt);
	harness.check(false);
      }

    pea.t = new ThreadDeath();
    try
      {
	AccessController.doPrivileged(pea);
      }
    catch (ThreadDeath td)
      {
	harness.check(true);
      }
    catch (Throwable tt)
      {
	harness.debug(tt);
	harness.check(false);
      }
  }

  public Object run() throws Exception
  {
    if (t instanceof Error)
      throw (Error) t;
    else
      throw (Exception) t;
  }
}
