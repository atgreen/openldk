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

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;

import java.util.HashMap;
import java.util.Map;

/**
 * Checks that {@link java.security.KeyPairGenerator#genKeyPair}
 * returns a key using the specified algorithm and key size.
 */
public class PR64902
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    Map<String,String> algNames = new HashMap<String,String>();

    // Standard algorithm request names and returned key names
    algNames.put("DiffieHellman", "DH");
    algNames.put("DSA", "DSA");
    algNames.put("RSA", "RSA");
    algNames.put("EC", "EC");

    for (Map.Entry<String,String> entry : algNames.entrySet())
      {
	try
	  {
	    String genName = entry.getKey();
	    String algName = entry.getValue();

	    KeyPairGenerator gen = KeyPairGenerator.getInstance(genName);
	    harness.check(gen.getAlgorithm(), genName,
			  "genKeyPair returns generator for " +
			  genName);
	    KeyPair pair = gen.genKeyPair();
	    harness.check(pair.getPublic().getAlgorithm(), algName,
		      "Returned public key uses requested algorithm " + algName);
	    harness.check(pair.getPrivate().getAlgorithm(), algName,
		      "Returned private key uses requested algorithm " + algName);
	  }
	catch (NoSuchAlgorithmException ex)
	  {
	    harness.debug(ex);
	    harness.fail("Algorithm not found: " + ex);
	  }
      }
  }
}
