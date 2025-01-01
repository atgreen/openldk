// Tags: not-a-test
// Copyright (c) 2000, 2001 NEC Corporation.

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


package gnu.testlet.org.omg.CORBA.ORB.RF11;

public class B
  implements org.omg.CORBA.portable.IDLEntity
{
  private int __value;
  private static int __size = 3;
  private static B[] __array = new B[ __size ];
  public static final int _b1 = 0;
  public static final B b1 = new B(_b1);
  public static final int _b2 = 1;
  public static final B b2 = new B(_b2);
  public static final int _b3 = 2;
  public static final B b3 = new B(_b3);

  public int value()
  {
    return __value;
  }

  public static B from_int(int value)
  {
    if (value >= 0 && value < __size)
      return __array [ value ];
    else
      throw new org.omg.CORBA.BAD_PARAM();
  }

  protected B(int value)
  {
    __value = value;
    __array [ __value ] = this;
  }
} // class B
