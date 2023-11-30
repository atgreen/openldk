// Regression test for InputStreamReader hang.

// Written by Tom Tromey <tromey@redhat.com>

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

// Tags: JDK1.4

package gnu.testlet.java.io.InputStreamReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;
import java.util.Arrays;

public class hang implements Testlet
{
  public void test (TestHarness h)
  {
    try
      {
	// We make a buffer where a multi-byte UTF-8 character is
	// carefully positioned so that the BufferedInputStream we create
	// will split it.
	byte[] bytes = new byte[20];
	Arrays.fill(bytes, (byte) 'a');
	bytes[9] = (byte) 208;
	bytes[10] = (byte) 164;

	ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
	BufferedInputStream bis = new BufferedInputStream(bais, 10);

	// Note that the encoding name matters for this particular
	// regression.  It must be exactly 'utf8'.
	InputStreamReader reader = new InputStreamReader(bis, "utf8");
	char[] result = new char[5];

	for (int i = 0; i < 4; ++i)
	  reader.read(result);

	h.check(true);
      }
    catch (IOException _)
      {
	h.debug(_);
	h.check(false);
      }
  }
}
