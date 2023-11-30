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

package gnu.testlet.java.security.Signature;

import java.security.SignatureSpi;
import java.security.PublicKey;
import java.security.PrivateKey;
import java.security.SignatureException;
import java.security.InvalidParameterException;
import java.security.InvalidKeyException;

// Dummy signature that just contains what was put in.
// And successfully verifies if the signature has a length > 2
public class MauveSignature extends SignatureSpi
{
  private byte[] signature;

  public Object engineGetParameter(String p)
    throws InvalidParameterException
  {
    throw new InvalidParameterException();
  }

  public void engineSetParameter(String p, Object o)
    throws InvalidParameterException
  {
    throw new InvalidParameterException();
  }

  public boolean engineVerify(byte[] sig) throws SignatureException
  {
    return sig.length > 2;
  }

  public byte[] engineSign() throws SignatureException
  {
    return signature;
  }

  public void engineUpdate(byte b)
  {
    signature = new byte[1];
    signature[0] = b;
  }

  public void engineUpdate(byte[] bs, int off, int len)
  {
    signature = new byte[len];
    System.arraycopy(bs, off, signature, 0, len);
  }

  public void engineInitSign(PrivateKey privateKey) throws InvalidKeyException
  {
  }

  public void engineInitVerify(PublicKey publicKey) throws InvalidKeyException
  {
  }
}
