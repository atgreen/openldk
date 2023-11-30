/* Resources.java -- Tests that the system class loader can get resources as 
 plain file and directory
 Copyright (C) 2006 Olivier Jolly <olivier.jolly@pcedev.com>
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

// Tags: JDK1.0


package gnu.testlet.java.lang.ClassLoader;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests which ensures that plain files and directories can be retrieved by the
 * system class loader.
 * @author Olivier Jolly <olivier.jolly@pcedev.com>
 * @see java.lang.ClassLoader#getResource(java.lang.String)
 */
public class Resources implements Testlet
{

  public void test(TestHarness harness)
  {
    harness.checkPoint("Resource loading");
    System.out.println(getClass().getClassLoader());

    try
      {
        getClass().getClassLoader().getResource(
                                                "gnu/testlet/java/lang/ClassLoader/Resources.class").getFile();
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.fail("Class resource should exist");
      }

    try
      {
        getClass().getClassLoader().getResource(
                                                "gnu/testlet/java/lang/ClassLoader/").getFile();
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.fail("Class directory should exist");
      }

    try
      {
        getClass().getClassLoader().getResource(
                                                "gnu/testlet/java/lang/ClassLoader").getFile();
        harness.check(true);
      }
    catch (Exception e)
      {
        harness.fail("Class directory should exist");
      }

  }

}
