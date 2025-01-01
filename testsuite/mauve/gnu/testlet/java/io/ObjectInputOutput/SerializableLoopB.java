/* SerializableLoopB.java -- Simple class used to create a loop
 Copyright (C) 2006 Olivier Jolly <olivier.jolly@pcedev.com>
 This file is part of Mauve.

 Mauve is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 Mauve is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Mauve; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

 */

// Tags: not-a-test


package gnu.testlet.java.io.ObjectInputOutput;

import java.io.Serializable;

public class SerializableLoopB implements Serializable
{

  private static final long serialVersionUID = 3033857304110309388L;

  SerializableLoopA a;

  int value = -1;

  public SerializableLoopA getA()
  {
    return a;
  }

  public void setA(SerializableLoopA a)
  {
    this.a = a;
  }

  public int getValue()
  {
    return value;
  }

  public void setValue(int value)
  {
    this.value = value;
  }

  public boolean equals(Object obj)
  {
    if (obj instanceof SerializableLoopB)
      {
        return getValue() == ((SerializableLoopB) obj).getValue();
      }
    return false;
  }

  public int hashCode()
  {
    return new Integer(getValue()).hashCode();
  }

}
