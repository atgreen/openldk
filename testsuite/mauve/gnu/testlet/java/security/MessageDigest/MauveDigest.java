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

package gnu.testlet.java.security.MessageDigest;

import java.security.MessageDigestSpi;

// Dummy digest that just contains what was put in.
public class MauveDigest extends MessageDigestSpi
{
  private byte[] digest;

  public void engineUpdate(byte b)
  {
    digest = new byte[1];
    digest[0] = b;
  }

  public void engineUpdate(byte[] bs, int off, int len)
  {
    digest = new byte[len];
    System.arraycopy(bs, off, digest, 0, len);
  }

  public byte[] engineDigest()
  {
    return digest;
  }

  public void engineReset()
  {
    digest = new byte[0];
  }
}

