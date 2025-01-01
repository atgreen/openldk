// Copyright (C) 2002 Free Software Foundation, Inc.
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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.
//
// Tags: JDK1.2

package gnu.testlet.java.lang.System;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class getProperty implements Testlet
{

  TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;

    getPropTest(null, "NullPointerException");
    getPropTest("", "IllegalArgumentException");
    getPropTest("__dummy_mauve_prop_not_set__", null);

    // setProperty is JDK 1.2
    System.setProperty("__dummy_mauve_prop_set__", "yes");
    getPropTest("__dummy_mauve_prop_set__", "yes");
  }

  void getPropTest(String key, String expect)
  {
    String result;

    try
     {
       result = System.getProperty(key);
     }
    catch (NullPointerException npe)
     {
       result = "NullPointerException";
     }
    catch (IllegalArgumentException iae)
     {
       result = "IllegalArgumentException";
     }
    catch (Throwable t)
     {
       result = t.toString();
     }

    harness.check(result, expect, "'"+key+"'");
  }
}
