// Tags: JDK1.1

// Copyright (C) 2005 Roman Kennke

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

package gnu.testlet.java.util.Properties;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Properties;

/**
 * Tests if getProperty(String) calls getProperty(String, String) or vice versa
 * or if both call an internal method. This is important to avoid infinite
 * recursion in derived classes that only implement one of these methods
 *
 * @author Roman Kennke
 */
public class getProperty implements Testlet
{
  static class Properties1 extends Properties
  {
    boolean called = false;
    public String getProperty(String key)
    {
      called = true;
      return super.getProperty(key);
    }
  }

  static class Properties2 extends Properties
  {
    boolean called = false;
    public String getProperty(String key, String def)
    {
      called = true;
      return super.getProperty(key, def);
    }
  }

  /**
   * This should not lead to infinite recursion.
   */
  static class Properties3 extends Properties
  {
    public String getProperty(String key, String def)
    {
      return getProperty(key + "." + def);
    }
  }

  public void test (TestHarness harness)
  {
    // First we override getProperty(String) and test if
    // getProperty(String, String)  calls this.
    Properties1 p1 = new Properties1();
    p1.setProperty("key", "value");
    p1.called = false;
    p1.getProperty("key", "default");
    harness.check(p1.called, true,
                  "getProperty(String, String) calls getProperty(String)");

    // Now we override getProperty(String, String) and test if
    // getProperty(String)  calls this.
    Properties2 p2 = new Properties2();
    p2.setProperty("key", "value");
    p2.called = false;
    p2.getProperty("key");
    harness.check(p2.called, false,
              "getProperty(String) does not call getProperty(String, String)");

    // Now we override getProperty(String, String) so that it calls
    // getProperty(String) and test if
    // getProperty(String, String) leads to infinite recursion.
    Properties3 p3 = new Properties3();
    p3.setProperty("key", "value");
    try
      {
        p3.getProperty("key", "def");
        harness.check(true, "overriding getProperty(String, String) must not "
                      + " lead to inifinite recursion.");
      }
    catch (Throwable ex)
      {
        harness.fail("overriding getProperty(String, String) must not "
                      + " lead to inifinite recursion.");
      }
  }

}
