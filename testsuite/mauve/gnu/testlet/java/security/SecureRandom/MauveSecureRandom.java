// Tags: not-a-test

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

import java.security.SecureRandomSpi;

// Dummy SecureRandom provider class that just returns the seed bytes.
public class MauveSecureRandom extends SecureRandomSpi
{
  private byte[] seed;

  protected void engineSetSeed(byte[] seed)
  {
    this.seed = seed;
  }

  public void engineNextBytes(byte[] random)
  {
    for (int i=0; i < random.length; i++)
      random[i] = seed[i];
  }

  public byte[] engineGenerateSeed(int n)
  {
    return seed;
  }
}
