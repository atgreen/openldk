// Tags: JDK1.2

// Copyright (C) 2015 Andrew John Hughes (gnu_andrew@member.fsf.org)

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

package gnu.testlet.java.security.KeyPairGenerator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.interfaces.RSAKey;

/**
 * Checks that {@link java.security.KeyPairGenerator#genKeyPair}
 * returns a key using the specified algorithm and key size.
 */
public class PR64881
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    try
      {
	KeyPairGenerator gen = KeyPairGenerator.getInstance("RSA");
	harness.check(gen.getAlgorithm(), "RSA",
		      "genKeyPair returns requested algorithm");
	gen.initialize(2048);
	PublicKey key = gen.genKeyPair().getPublic();
	harness.check(key.getAlgorithm(), "RSA",
		      "Returned key uses requested algorithm");
	if (key instanceof RSAKey)
	  harness.check(((RSAKey) key).getModulus().bitLength(),
			2048, "Returned key is of specified length");
      }
    catch (NoSuchAlgorithmException ex)
      {
	harness.debug(ex);
	harness.fail("Algorithm not found: " + ex);
      }
  }
}
