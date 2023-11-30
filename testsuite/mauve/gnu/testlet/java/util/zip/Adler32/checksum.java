// Tags: JDK1.1

// Copyright (C) 2005 Mark J. Wielaard (mark@klomp.org)

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
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.util.zip.Adler32;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.zip.*;

public class checksum implements Testlet
{
  // 1000, 6000, ..., 96000 arrays filled with value = (byte) index.
  private final long[] someMore = new long[] { 486795068L,
					       1525910894L,
					       3543032800L,
					       2483946130L,
					       4150712693L,
					       3878123687L,
					       3650897945L,
					       1682829244L,
					       1842395054L,
					       460416992L,
					       3287492690L,
					       479453429L,
					       3960773095L,
					       2008242969L,
					       4130540683L,
					       1021367854L,
					       4065361952L,
					       2081116754L,
					       4033606837L,
					       1162071911L };

  public void test(TestHarness harness)
  {
    byte[] bs;
    for (int i = 0; i < 20; i++)
      {
	int length = i * 5000 + 1000;
	bs = new byte[length];
	for (int j = 0; j < bs.length; j++)
	  bs[j] = (byte) j;
	test(harness, bs, someMore[i]);
      }
  }

  private void test(TestHarness harness, byte[] bs, long result)
  {
    Adler32 adler = new Adler32();
    harness.check(adler.getValue(), 1);
    adler.update(bs);
    harness.check(adler.getValue(), result);

    adler.reset();
    harness.check(adler.getValue(), 1);
    for (int i = 0; i < bs.length; i += 1000)
      adler.update(bs, i, 1000);
    harness.check(adler.getValue(), result);

    adler.reset();
    harness.check(adler.getValue(), 1);
    for (int i = 0; i < bs.length; i++)
      adler.update(bs[i]);
    harness.check(adler.getValue(), result);

    adler.reset();
    harness.check(adler.getValue(), 1);
    for (int i = 0; i < 250; i++)
      adler.update(bs[i]);
    for (int i = 250; i < bs.length - 250; i += 250)
      adler.update(bs, i, 250);
    for (int i = bs.length - 250; i < bs.length; i++)
      adler.update(bs[i]);
    harness.check(adler.getValue(), result);
  }
}
