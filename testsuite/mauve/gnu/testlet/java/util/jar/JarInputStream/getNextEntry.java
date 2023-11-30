// Tags: JDK1.1

// Copyright (C) 2003 Mark Wielaard

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

package gnu.testlet.java.util.jar.JarInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.jar.*;
import java.io.*;

public class getNextEntry implements Testlet
{
  public void test (TestHarness harness)
  {
    try
      {
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	JarOutputStream jos = new JarOutputStream(baos);
	JarEntry entry = new JarEntry("test-entry");
	jos.putNextEntry(entry);
	jos.write(1);
	jos.write(2);
	jos.write(3);
	jos.close();
	baos.close();
	byte[] bs = baos.toByteArray();

	ByteArrayInputStream bais = new ByteArrayInputStream(bs);
	JarInputStream jis = new JarInputStream(bais);
	entry = jis.getNextJarEntry();
	harness.check(entry.getName(), "test-entry");
	harness.check(jis.read(), 1);
	harness.check(jis.read(), 2);
	harness.check(jis.read(), 3);
	harness.check(jis.read(), -1);
	jis.close();
	bais.close();
      }
    catch (Throwable t)
      {
	harness.debug(t);
	harness.check(false, "Unexpected IOException");
      }
  }
}
