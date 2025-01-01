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

package gnu.testlet.java.security.AlgorithmParameters;
import java.io.IOException;
import java.security.AlgorithmParametersSpi;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.InvalidParameterSpecException;

public class MauveAlgorithm extends AlgorithmParametersSpi
{
  protected void engineInit (AlgorithmParameterSpec paramSpec)
      throws InvalidParameterSpecException
  {
  }

  protected void engineInit (byte[] params) throws IOException
  {
  }

  protected void engineInit (byte[] params, String format) throws IOException
  {
  }

  protected
  AlgorithmParameterSpec engineGetParameterSpec (Class paramSpec)
      throws InvalidParameterSpecException
  {
    return null;
  }

  protected byte[] engineGetEncoded () throws IOException
  {
    return new byte[0];
  }

  protected byte[] engineGetEncoded (String format) throws IOException
  {
    return new byte[0];
  }

  protected String engineToString ()
  {
    return null;
  }
}
