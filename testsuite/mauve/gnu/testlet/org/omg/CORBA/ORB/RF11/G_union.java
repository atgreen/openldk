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

public final class G_union
  implements org.omg.CORBA.portable.IDLEntity
{
  private E_struct ___e_e_struct;
  private E_union ___e_e_union;
  private B[] ___e_e_sequence;
  private B[] ___e_e_array;
  private int __discriminator;
  private boolean __uninitialized = true;

  public G_union()
  {
  }

  public int discriminator()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    return __discriminator;
  }

  public E_struct e_e_struct()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_e_struct(__discriminator);
    return ___e_e_struct;
  }

  public void e_e_struct(E_struct value)
  {
    __discriminator = 1;
    ___e_e_struct = value;
    __uninitialized = false;
  }

  private void verifye_e_struct(int discriminator)
  {
    if (discriminator != 1)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public E_union e_e_union()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_e_union(__discriminator);
    return ___e_e_union;
  }

  public void e_e_union(E_union value)
  {
    __discriminator = 2;
    ___e_e_union = value;
    __uninitialized = false;
  }

  private void verifye_e_union(int discriminator)
  {
    if (discriminator != 2)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public B[] e_e_sequence()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_e_sequence(__discriminator);
    return ___e_e_sequence;
  }

  public void e_e_sequence(B[] value)
  {
    __discriminator = 3;
    ___e_e_sequence = value;
    __uninitialized = false;
  }

  private void verifye_e_sequence(int discriminator)
  {
    if (discriminator != 3)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public B[] e_e_array()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_e_array(__discriminator);
    return ___e_e_array;
  }

  public void e_e_array(B[] value)
  {
    __discriminator = 4;
    ___e_e_array = value;
    __uninitialized = false;
  }

  private void verifye_e_array(int discriminator)
  {
    if (discriminator != 4)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public void _default()
  {
    __discriminator = -2147483648;
    __uninitialized = false;
  }
} // class G_union
