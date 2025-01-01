/* constructor.java 
   Copyright (C) 2007, 2012 Red Hat
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4

package gnu.testlet.java.awt.AWTPermission;

import java.awt.AWTPermission;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class constructor implements Testlet
{

  public void test(TestHarness harness)
  {
    AWTPermission permission = new AWTPermission("String");
    harness.check(permission.getActions(), "");
    harness.check(permission.getName(), "String");
    if (conformToJDK17())
      {
        harness.check(permission.toString(), "(\"java.awt.AWTPermission\" \"String\")");
      }
    else
      {
        harness.check(permission.toString(), "(java.awt.AWTPermission String)");
      }
    
    // String cannot be the empty string.
    boolean fail = false;
    try
      {
        permission = new AWTPermission("");
      }
    catch (IllegalArgumentException e)
      {
        fail = true;
      }
    harness.check(fail);
    
    // Name cannot be null.
    fail = false;
    try
      {
        permission = new AWTPermission(null);
      }
    catch (NullPointerException e)
      {
        fail = true;
      }
    harness.check(fail);
  }

  /**
    * Returns true if tested JRE conformns to JDK 1.7.
    * @author: Mark Wielaard
    */
  private static boolean conformToJDK17()
  {
    String[] javaVersion = System.getProperty("java.version").split("\\.");
    String vendorID = System.getProperty("java.vendor");
    // test of OpenJDK
    if ("Sun Microsystems Inc.".equals(vendorID))
      {
        return Long.parseLong(javaVersion[1]) >= 7;
      }
    return true;
  }

}
