// Tags: not-a-test
// Copyright (c) IONA Technologies, 2001.

// Adapted for Mauve by Audrius Meskauskas <audriusa@bluewin.ch>

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
// Boston, MA 02111-1307, USA.

/*
This code originally came from the OMG's CORBA Open Source Testing project,
which lived at cost.omg.org. That site no longer exists.

All the contributing companies agreed to release their tests under the
terms of the GNU Lesser General Public License, available in the file
COPYING.LIB.

The code has been modified integrating into Mauve test environment and
removing tests that are not yet supported by Suns jre 1.4. Hence the license
is now GPL.

We downloaded the code from http://sourceforge.net/projects/corba-cost/,
administrated by Duncan Grisby.
*/

package gnu.testlet.org.omg.DynamicAny.DynAny.Iona;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.portable.IDLEntity;

public class TestEnum
  implements IDLEntity
{
  static int m_size = 3;
  static TestEnum[] m_array = new TestEnum[ m_size ];
  static int m_red = 0;
  static int m_blue = 2;
  static int m_green = 1;
  public static TestEnum red = new TestEnum(m_red);
  public static TestEnum green = new TestEnum(m_green);
  public static TestEnum blue = new TestEnum(m_blue);
  int m_value;

  protected TestEnum(int value)
  {
    m_value = value;
    m_array [ m_value ] = this;
  }

  public static TestEnum from_int(int value)
  {
    if (value >= 0 && value < m_size)
      {
        return m_array [ value ];
      }
    else
      {
        throw new BAD_PARAM();
      }
  }

  public int value()
  {
    return m_value;
  }
}
