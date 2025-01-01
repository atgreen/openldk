//Tags: not-a-test

//Copyright (C) 2005 Free Software Foundation, Inc.
//Written by Wolfgang Baer (WBaer@gmx.de)

//This file is part of Mauve.

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */


package gnu.testlet.java.io.ObjectInputStream;

import java.io.InvalidObjectException;
import java.io.ObjectStreamException;
import java.io.Serializable;

/**
 * Helper class for readResolve. Provides a readResolve method and depending on
 * the value given to the constructor throws different error and exception types
 * for testing or returns successfully in the default case.
 */
public class ReadResolveHelper implements Serializable
{
  public int value;

  public ReadResolveHelper(int value)
  {
    this.value = value;
  }

  protected Object readResolve() throws ObjectStreamException
  {
    switch (value)
      {
      case 1: // any error
        throw new Error();
      case 2: // runtime exception
        throw new RuntimeException("RuntimeException");
      case 3: // objectstreamexception
        throw new InvalidObjectException("InvalidObjectException");
      default:
        return new ReadResolveHelper(4);
      }
  }

}