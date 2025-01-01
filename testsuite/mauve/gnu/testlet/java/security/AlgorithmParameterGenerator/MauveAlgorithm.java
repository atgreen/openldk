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

package gnu.testlet.java.security.AlgorithmParameterGenerator;
import java.security.AlgorithmParameterGeneratorSpi;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.SecureRandom;
import java.security.spec.AlgorithmParameterSpec;

public class MauveAlgorithm extends AlgorithmParameterGeneratorSpi
{
  protected void engineInit (int size, SecureRandom random)
  {
  }

  protected void engineInit (AlgorithmParameterSpec genParamSpec,
                             SecureRandom random)
      throws InvalidAlgorithmParameterException
  {
  }

  protected AlgorithmParameters engineGenerateParameters ()
  {
    return null;
  }
}
