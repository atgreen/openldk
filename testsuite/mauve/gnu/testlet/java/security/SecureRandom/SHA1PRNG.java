// Tags: JDK1.2

// Copyright (C) 2003 Free Software Foundation, Inc.

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

package gnu.testlet.java.security.SecureRandom;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.security.SecureRandom;
import java.security.NoSuchAlgorithmException;
//import java.security.NoSuchProviderException;

public class SHA1PRNG implements Testlet
{
  private TestHarness harness = null;
  boolean available = false;

  public void test (TestHarness harness)
  {
    this.harness = harness;

    this.harness.checkPoint ("SHA1PRNG");
    instanceTest ();
    setSeeedTest ();
  }

  // .../docs/guide/security/HowToImplAProvider.html lists SHA1PRNG as a MUST
  private void instanceTest ()
  {
    available = (getInstance () != null);
    harness.check (available, "found implementation");
  }

  // SecureRandom javadoc states:
  //
  // The SecureRandom implementation attempts to completely randomize the
  // internal state of the generator itself unless the caller follows the call
  // to a getInstance method with a call to the setSeed method:
  //
  //    SecureRandom random = SecureRandom.getInstance("SHA1PRNG");
  //    random.setSeed(seed);
  //
  // After the caller obtains the SecureRandom object from the getInstance call,
  // it can call nextBytes to generate random bytes:
  private void setSeeedTest ()
  {
    if (!available)
      harness.fail ("no implementation found");
    else
      {
        long a, b;
        SecureRandom prng1 = getInstance ();
        prng1.setSeed (98243647L);
        SecureRandom prng2 = getInstance ();
        prng2.setSeed (98243647L);

        a = prng1.nextLong();
        b = prng2.nextLong();
        harness.check (a == b,
                      "instances generate same bytes when similarly seeded");

        // if true in the beginning, it should be so forever
        for (int i = 0; i < 1000; i++)
          {
            prng1.nextLong();
            prng2.nextLong();
          }

        a = prng1.nextLong();
        b = prng2.nextLong();
        harness.check (a == b);
      }
  }

  private SecureRandom getInstance ()
  {
    SecureRandom result = null;
    try
      {
	result = SecureRandom.getInstance ("SHA1PRNG");
//	result = SecureRandom.getInstance ("SHA1PRNG", "GNU");
      }
//    catch (NoSuchProviderException x)
//      {
//        harness.debug (x);
//      }
    catch (NoSuchAlgorithmException x)
      {
        harness.debug (x);
      }
    return result;
  }
}

