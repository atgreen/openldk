// Tags: JDK1.2
//
// Uses: MauveSecureRandom

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Mark Wielaard (mark@klomp.org)

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

import java.security.Security;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.GeneralSecurityException;

public class Instance extends Provider
  implements Testlet
{
  static final String NAME = "Mauve-Test-Provider-SecureRandom";
  static final double VERSION = 3.14;
  static final String INFO = "Mauve Info-Test implements MauveSecureRandom";

  TestHarness harness;

  public Instance()
  {
    super(NAME, VERSION, INFO);

    put("SecureRandom.MauveSecureRandom",
	"gnu.testlet.java.security.SecureRandom.MauveSecureRandom");
    put("Alg.Alias.SecureRandom.MauveAlias", "MauveSecureRandom");
  }

  void checkSecureRandom(String name)
  {
    checkSecureRandom(name, null);
  }

  void checkSecureRandom(String name, String provider)
  {
    String checkPoint = name + (provider == null ? "" : " " + provider);
    harness.checkPoint(checkPoint);

    SecureRandom sr;
    try
      {
	if (provider == null)
	  sr = SecureRandom.getInstance(name);
	else
	  sr = SecureRandom.getInstance(name, provider);
      }
    catch (GeneralSecurityException gse)
      {
	harness.fail(checkPoint + " instance caught " + gse);
	return;
      }

    // Just make sure we got the correct Signature 
    harness.check(sr.getProvider(), this);

    // Do some of our dummy operations
    byte[] seed = new byte[1];
    seed[0] = 42;
    sr.setSeed(seed);
    byte[] random = new byte[1];
    sr.nextBytes(random);
    harness.check(random[0], (byte)42);
  }

  public void test (TestHarness h)
  {
    this.harness = h;

    // Without provider
    byte[] seed = new byte[1];
    seed[0] = 42;
    SecureRandom sr = new SecureRandom();
    harness.check(sr != null);
    sr = new SecureRandom(seed);
    harness.check(sr != null);

    // With our own provider
    Security.addProvider(this);
    sr = new SecureRandom();
    harness.check(sr != null);
    sr = new SecureRandom(seed);
    harness.check(sr != null);

    checkSecureRandom("MauveSecureRandom");
    checkSecureRandom("MAUVESecurerandom");
    checkSecureRandom("MauveAlias");
    checkSecureRandom("MAUVEALIAS");

    checkSecureRandom("MauveSecureRandom", NAME);
    checkSecureRandom("MAUVESECURERANDOM", NAME);
    checkSecureRandom("MauveAlias", NAME);
    checkSecureRandom("MAUVEALIAS", NAME);
  }
}

