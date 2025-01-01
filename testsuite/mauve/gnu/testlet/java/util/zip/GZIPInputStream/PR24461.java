/* PR24461.java -- Regression test for PR 24461
   Copyright (C) 2006 jrandom <jrandom-gcc@i2p.net>
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

// Tags: JDK1.1

package gnu.testlet.java.util.zip.GZIPInputStream;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Random;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class PR24461 implements Testlet {
	public void test(TestHarness harness) {
		boolean canBeOk = false;
		boolean ok = false;
		try {
	        ByteArrayOutputStream full = new ByteArrayOutputStream(1024);
	        GZIPOutputStream gzout = new GZIPOutputStream(full);
	        byte buf[] = new byte[1024];
	        new Random().nextBytes(buf);
	        gzout.write(buf);
	        gzout.close();
	        byte gzdata[] = full.toByteArray();

	        // now only read the first 128 bytes of that data
	        ByteArrayInputStream truncated = new ByteArrayInputStream(gzdata, 0, 128);
	        GZIPInputStream gzin = new GZIPInputStream(truncated);
	        byte read[] = new byte[1024];
	        int cur = 0;
	        canBeOk = true;
	        while ( (cur = gzin.read(read, cur, read.length-cur)) != -1)
	          ; //noop
		  } catch (IOException ioe) {
			  // We expect an IOException while reading the truncated stream.
			  // The bug was that we were seeing a NullPointerException.
			  ok = canBeOk;
	      } catch (Exception e) {
	        harness.debug(e);
	      }
	      harness.check(ok);
	}
}
