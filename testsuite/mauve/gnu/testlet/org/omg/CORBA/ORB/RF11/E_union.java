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

public final class E_union
  implements org.omg.CORBA.portable.IDLEntity
{
  private B ___e_b1;
  private B ___e_b2;
  private int __discriminator;
  private boolean __uninitialized = true;

  public E_union()
  {
  }

  public int discriminator()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    return __discriminator;
  }

  public B e_b1()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_b1(__discriminator);
    return ___e_b1;
  }

  public void e_b1(B value)
  {
    __discriminator = 1;
    ___e_b1 = value;
    __uninitialized = false;
  }

  private void verifye_b1(int discriminator)
  {
    if (discriminator != 1)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public B e_b2()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_b2(__discriminator);
    return ___e_b2;
  }

  public void e_b2(B value)
  {
    __discriminator = 2;
    ___e_b2 = value;
    __uninitialized = false;
  }

  private void verifye_b2(int discriminator)
  {
    if (discriminator != 2)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public void _default()
  {
    __discriminator = -2147483648;
    __uninitialized = false;
  }
} // class E_union
