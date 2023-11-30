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

public final class C_array_e_booleanHolder
  implements org.omg.CORBA.portable.Streamable
{
  public boolean[] value = null;

  public C_array_e_booleanHolder()
  {
  }

  public C_array_e_booleanHolder(boolean[] initialValue)
  {
    value = initialValue;
  }

  public void _read(org.omg.CORBA.portable.InputStream i)
  {
    value = C_array_e_booleanHelper.read(i);
  }

  public void _write(org.omg.CORBA.portable.OutputStream o)
  {
    C_array_e_booleanHelper.write(o, value);
  }

  public org.omg.CORBA.TypeCode _type()
  {
    return C_array_e_booleanHelper.type();
  }
}
