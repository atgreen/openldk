// Tags: JDK1.1

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.security.Security;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.Security;

public class property implements Testlet
{
  public void test (TestHarness harness)
  {
    // Nothing much specified for this method...

    String key = "Mauve-Key-test-prop";
    String value = key + "-value";

    harness.check(Security.getProperty(key), null);
    Security.setProperty(key, value);
    harness.check(Security.getProperty(key), value);
    Security.setProperty(key, null);
    harness.check(Security.getProperty(key), null);
  }
}

