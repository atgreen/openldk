// Tags: JDK1.2

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Mark Wielaard (mark@klomp.org)
// Based on a test by Archie Cobbs (archie@dellroad.org)

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

package gnu.testlet.java.security.DigestInputStream;

import java.io.*;
import java.security.*;

import gnu.testlet.*;

public class readMD5 implements Testlet
{
  // echo -n "foobar" | md5sum
  private static String md5 = "3858f62230ac3c915f300c664312c63f";

  public void test (TestHarness harness)
  {
    try
      {
	byte[] foobar = "foobar".getBytes("UTF-8");
	ByteArrayInputStream bais = new ByteArrayInputStream(foobar);
	MessageDigest md5Digest = MessageDigest.getInstance("MD5");
	DigestInputStream dis = new DigestInputStream(bais, md5Digest);

	byte[] buf = new byte[100];
	while (dis.read(buf, 0, buf.length) != -1);
	byte[] hash = dis.getMessageDigest().digest();
	
	StringBuffer result = new StringBuffer();
	for (int i = 0; i < hash.length; i++)
	  {
	    result.append(Integer.toHexString((hash[i] >> 4) & 0xf));
	    result.append(Integer.toHexString(hash[i] & 0xf));
	  }
	harness.check(result.toString(), md5);
      }
    catch (Throwable t)
      {
	harness.debug(t);
	harness.check(false, t.toString());
      }
  }
}
