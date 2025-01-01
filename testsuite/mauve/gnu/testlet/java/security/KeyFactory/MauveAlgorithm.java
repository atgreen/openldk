// $Id: MauveAlgorithm.java,v 1.2 2004/05/02 12:48:24 aph Exp $
//
// Copyright (C) 2003, Free Software Foundation, Inc.
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.
//
// Tags: not-a-test

package gnu.testlet.java.security.KeyFactory;
import java.security.KeyFactorySpi;
import java.security.PublicKey;
import java.security.PrivateKey;
import java.security.Key;
import java.security.InvalidKeyException;
import java.security.spec.KeySpec;
import java.security.spec.InvalidKeySpecException;

public class MauveAlgorithm extends KeyFactorySpi
{
  protected PublicKey engineGeneratePublic (KeySpec keySpec)
      throws InvalidKeySpecException
  {
    return null;
  }

  protected PrivateKey engineGeneratePrivate (KeySpec keySpec)
      throws InvalidKeySpecException
  {
    return null;
  }

  protected KeySpec engineGetKeySpec (Key key, Class keySpec)
      throws InvalidKeySpecException
  {
    return null;
  }

  protected Key engineTranslateKey (Key key)
      throws InvalidKeyException
  {
    return null;
  }
}
