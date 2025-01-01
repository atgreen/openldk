// Tags: JDK1.0

// Copyright (C) 1998, 2002 Cygnus Solutions

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

package gnu.testlet.java.util.Random;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Random;

public class basic implements Testlet
{
  public void test (TestHarness harness)
    {
      Random rand;

      rand = new Random(122760);
      harness.check (rand.nextInt(), -1524104671);

      harness.check (rand.nextLong(), 2785759620113032781L);

      harness.check (String.valueOf(rand.nextDouble()), "0.8173322904425151");

      harness.check (String.valueOf(rand.nextFloat()), "0.8239248");

      byte[] b = new byte[0];
      rand.nextBytes(b);
      harness.check (rand.nextInt(), -899478426);

      rand = new Random(122760);
      rand.nextInt();
      rand.nextLong();
      rand.nextDouble();
      rand.nextFloat();
      b = new byte[3];
      rand.nextBytes(b);
      harness.check (b[0], 102);
      harness.check (b[1], 12);
      harness.check (b[2], 99);
      harness.check (rand.nextInt(), -1550323395);

      rand = new Random(122760);
      rand.nextInt();
      rand.nextLong();
      rand.nextDouble();
      rand.nextFloat();
      b = new byte[4];
      rand.nextBytes(b);
      harness.check (b[0], 102);
      harness.check (b[1], 12);
      harness.check (b[2], 99);
      harness.check (b[3], -54);
      harness.check (rand.nextInt(), -1550323395);

      rand = new Random(122760);
      rand.nextInt();
      rand.nextLong();
      rand.nextDouble();
      rand.nextFloat();
      b = new byte[5];
      rand.nextBytes(b);
      harness.check (b[0], 102);
      harness.check (b[1], 12);
      harness.check (b[2], 99);
      harness.check (b[3], -54);
      harness.check (b[4], 61);
      harness.check (rand.nextInt(), -270809961);

      // Spot check for negative numbers.  This is a regression test
      // an old Classpath bug.
      boolean ok = true;
      rand = new Random (0);
      for (int i=0; i < 1000000; ++i)
	{
	  int x = rand.nextInt (1000);
	  if (x < 0 || x >= 1000)
	    ok = false;
	}
      harness.check (ok);
    }
}
