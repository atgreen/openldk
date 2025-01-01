// Tags: JDK1.1

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

package gnu.testlet.java.security.Security;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.security.Security;
import java.security.Provider;

public class provider extends Provider implements Testlet
{
  static final String NAME = "Mauve-Test-Provider";
  static final double VERSION = 3.14;
  static final String INFO = "Mauve Info-Test";

  public provider()
  {
    super(NAME, VERSION, INFO);
  }

  provider(int i)
  {
    super(NAME + i, VERSION, INFO);
  }

  public void test (TestHarness harness)
  {
    // Backup original providers.
    Provider[] orig_providers = Security.getProviders();
    harness.check(orig_providers != null);

    // Add a new provider.
    Provider p1 = new provider(1);
    int pos1 = Security.addProvider(p1);
    harness.check(pos1 != -1);

    // Is it inserted?
    Provider[] new_providers = Security.getProviders();
    harness.check(orig_providers.length + 1, new_providers.length);

    // In the correct place?
    harness.check(new_providers[pos1-1], p1);

    // Add another, should be after 1
    Provider p2 = new provider(2);
    int pos2 = Security.addProvider(p2);
    harness.check(pos2-1, pos1);

    // All in the correct place?
    new_providers = Security.getProviders();
    harness.check(orig_providers.length + 2, new_providers.length);
    
    harness.check(new_providers[pos1-1], p1);
    harness.check(new_providers[pos2-1], p2);

    // Add new one in front, note 1 based.
    Provider p0 = new provider(0);
    int pos0 = Security.insertProviderAt(p0, 1);
    harness.check(pos0 != -1);

    // Cannot check if pos was respected because that is not guaranteed.

    // All in the correct place?
    new_providers = Security.getProviders();
    harness.check(orig_providers.length + 3, new_providers.length);
    harness.check(new_providers[pos0-1], p0);

    // Are they all there?
    harness.check(Security.getProvider(p0.getName()), p0);
    harness.check(Security.getProvider(p1.getName()), p1);
    harness.check(Security.getProvider(p2.getName()), p2);

    // No Unknown ones
    harness.check(Security.getProvider("UNKNOWN " + NAME + "42"), null);
    
    // Re-adding providers will fail
    harness.check(Security.addProvider(p1), -1);
    harness.check(Security.addProvider(p2), -1);

    harness.check(Security.insertProviderAt(p1,1), -1);
    harness.check(Security.insertProviderAt(p2,2), -1);

    // You may remove as much as you want
    Security.removeProvider(p0.getName());
    Security.removeProvider(p2.getName());
    Security.removeProvider("UNKNOWN " + NAME + "42");
    Security.removeProvider(p2.getName());
    Security.removeProvider(p0.getName());

    // Gone?
    harness.check(Security.getProvider(p0.getName()), null);
    harness.check(Security.getProvider(p2.getName()), null);

    // Provider 1 still at original place?
    harness.check(Security.getProvider(p1.getName()), p1);
    new_providers = Security.getProviders();
    harness.check(new_providers[pos1-1], p1);

    // Done
    Security.removeProvider(p1.getName());
    new_providers = Security.getProviders();
    harness.check(new_providers.length, orig_providers.length);
  }
}

