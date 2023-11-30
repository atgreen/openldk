// Test for InputStreamReader exception handling.

// Written by paul@dawa.demon.co.uk

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

// Tags: JDK1.1

package gnu.testlet.java.io.InputStreamReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

public class except implements Testlet
{
  public void test (TestHarness harness)
  {
    boolean ok = false;
    try
      {
	InputStreamReader isr = new InputStreamReader (new StringBufferInputStream ("zardoz has spoken"));

	char[] cbuf = new char[10];

	isr.close ();
	isr.read (cbuf, 0, 9);
      }
    catch (IOException _1)
      {
	// This is expected.
	ok = true;
      }
    catch (Throwable _2)
      {
	// Failure.
	harness.debug(_2);
      }

    harness.check (ok);
  }
}
