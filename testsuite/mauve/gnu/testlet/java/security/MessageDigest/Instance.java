// Tags: JDK1.2
//
// Uses: MauveDigest

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

package gnu.testlet.java.security.MessageDigest;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.Security;
import java.security.Provider;
import java.security.MessageDigest;
import java.security.GeneralSecurityException;

public class Instance extends Provider implements Testlet
{
  static final String NAME = "Mauve-Test-Provider-Digest";
  static final double VERSION = 3.14;
  static final String INFO = "Mauve Info-Test implements MauveDigest";

  TestHarness harness;

  public Instance()
  {
    super(NAME, VERSION, INFO);

    put("MessageDigest.MauveDigest",
	"gnu.testlet.java.security.MessageDigest.MauveDigest");
    put("Alg.Alias.MessageDigest.MauveAlias", "MauveDigest");
  }

  void checkDigest(String name)
  {
    checkDigest(name, null);
  }

  void checkDigest(String name, String provider)
  {
    String checkPoint = name + (provider == null ? "" : " " + provider);
    harness.checkPoint(checkPoint);

    MessageDigest d;
    try
      {
	if (provider == null)
	  d = MessageDigest.getInstance(name);
	else
	  d = MessageDigest.getInstance(name, provider);
      }
    catch (GeneralSecurityException gse)
      {
	harness.fail(checkPoint + " caught " + gse);
	return;
      }

    // Just make sure we got the correct MessageDigest
    harness.check(d.getAlgorithm(), name);
    harness.check(d.getProvider(), this);

    // Do some of our dummy operations
    d.reset();
    byte[] digest;
    digest = d.digest();
    harness.check(digest.length, 0);

    byte[] message = new byte[] {0, 1, 2, 3};
    digest = d.digest(message);
    harness.check(MessageDigest.isEqual(digest, message));

    d.update((byte)6);
    byte[] bs = d.digest();
    harness.check(bs[0], (byte)6);
  }

  public void test (TestHarness h)
  {
    this.harness = h;
    Security.addProvider(this);

    checkDigest("MauveDigest");
    checkDigest("MAUVEDIGEST");
    checkDigest("MauveAlias");
    checkDigest("MAUVEALIAS");

    checkDigest("MauveDigest", NAME);
    checkDigest("MAUVEDIGEST", NAME);
    checkDigest("MauveAlias", NAME);
    checkDigest("MAUVEALIAS", NAME);
  }
}

