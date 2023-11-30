// Tags: JDK1.2
//
// Uses: MauveSignature

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

package gnu.testlet.java.security.Signature;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.Security;
import java.security.Provider;
import java.security.Signature;
import java.security.PublicKey;
import java.security.PrivateKey;
import java.security.GeneralSecurityException;

public class Instance extends Provider
  implements Testlet
{
  static final String NAME = "Mauve-Test-Provider-Signature";
  static final double VERSION = 3.14;
  static final String INFO = "Mauve Info-Test implements MauveSignature";

  TestHarness harness;

  public Instance()
  {
    super(NAME, VERSION, INFO);

    put("Signature.MauveSignature",
	"gnu.testlet.java.security.Signature.MauveSignature");
    put("Alg.Alias.Signature.MauveAlias", "MauveSignature");
  }

  void checkSignature(String name)
  {
    checkSignature(name, null);
  }

  void checkSignature(String name, String provider)
  {
    String checkPoint = name + (provider == null ? "" : " " + provider);
    harness.checkPoint(checkPoint);

    Signature s;
    try
      {
	if (provider == null)
	  s = Signature.getInstance(name);
	else
	  s = Signature.getInstance(name, provider);
      }
    catch (GeneralSecurityException gse)
      {
	harness.fail(checkPoint + " instance caught " + gse);
	return;
      }

    // Just make sure we got the correct Signature 
    harness.check(s.getAlgorithm(), name);
    harness.check(s.getProvider(), this);

    // Do some of our dummy operations
    try
      {
	s.initSign((PrivateKey)null);
	s.update((byte)6);
	byte[] sign = s.sign();
	harness.check(sign[0], (byte)6);

	s.initVerify((PublicKey)null);
	byte[] message = new byte[] {0, 1, 2, 3};
	s.update(message);
	harness.check(s.verify(message));
      }
    catch (GeneralSecurityException gse)
      {
	harness.fail(checkPoint + " dummy caught " + gse);
      }
  }

  public void test (TestHarness h)
  {
    this.harness = h;
    Security.addProvider(this);

    checkSignature("MauveSignature");
    checkSignature("MAUVESignature");
    checkSignature("MauveAlias");
    checkSignature("MAUVEALIAS");

    checkSignature("MauveSignature", NAME);
    checkSignature("MAUVESIGNATURE", NAME);
    checkSignature("MauveAlias", NAME);
    checkSignature("MAUVEALIAS", NAME);
  }
}

