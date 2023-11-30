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
import java.security.Provider;
import java.security.Security;
import java.util.Set;

import static java.security.Provider.Service;

/**
 * Checks that {@link java.security.KeyPairGenerator#genKeyPair}
 * returns a key using the specified algorithm and key size.
 */
public class PR64904
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   *
   * @param harness the test harness.
   */
  public void test(TestHarness harness)
  {
    Set<Service> services = null;
    Provider[] providers = Security.getProviders();
    for (Provider provider : providers)
      {
	String provName = provider.getName();
	harness.debug("Provider " + provName + ": "
		      + provider.getInfo());
	services = provider.getServices();
	for (Service service : services)
	  {
	    if (service.getType().equals("KeyPairGenerator"))
	      {
		String algName = service.getAlgorithm();
		String desiredName = algName;
		// Workaround DiffieHellman / DH mismatch
		if (algName.equals("DiffieHellman"))
		    desiredName = "DH";
		try
		  {
		    harness.debug("Requesting " + algName + " from " + provName);
		    KeyPairGenerator gen = KeyPairGenerator.getInstance(algName,
									provider);
		    KeyPair pair = gen.genKeyPair();
		    harness.check(pair.getPublic().getAlgorithm(), desiredName,
				  "Provider " + provName +
				  " returned public key using requested algorithm "
				  + desiredName);
		    harness.check(pair.getPrivate().getAlgorithm(), desiredName,
				  "Provider " + provName +
				  " returned private key using requested algorithm "
				  + desiredName);
		  }
		catch (NoSuchAlgorithmException ex)
		  {
		    harness.debug(ex);
		    harness.fail("Algorithm not found: " + ex);
		  }	
	      }
	  }
      }
  }
}
