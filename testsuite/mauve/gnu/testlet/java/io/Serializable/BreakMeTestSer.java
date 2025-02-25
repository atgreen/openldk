// Tags: JDK1.4
// Uses: BreakMe MyBreakMe

// Copyright (C) 2005, 2025 Mark J. Wielaard <mark@klomp.org>
// Based on an idea by Jeroen Frijters

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.io.Serializable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.*;

/**
 * Tests that errors thrown during serialization are propagated correctly.
 * BreakMe.ser can be generated by running the main() method.
 */
public class BreakMeTestSer implements Testlet
{
  public void test(TestHarness harness)
  {
    // This will partly fail since BreakMe.generating isn't set.
    try { new MyBreakMe(); } catch(Throwable _xx) {}

    boolean exception_thrown = false;
    try
      {
	OutputStream os = new ByteArrayOutputStream();
	ObjectOutputStream oos = new ObjectOutputStream(os);
	oos.writeObject(BreakMe.broken);
	oos.close();
      }
    catch (NoClassDefFoundError ncdfe)
      {
	exception_thrown = true;
      }
    catch (Throwable t)
      {
	harness.debug(t);
      }
    harness.check(exception_thrown);
  }
}
