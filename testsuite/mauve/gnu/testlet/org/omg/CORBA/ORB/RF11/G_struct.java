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

// G
package gnu.testlet.org.omg.CORBA.ORB.RF11;

public final class G_struct
  implements org.omg.CORBA.portable.IDLEntity
{
  public E_struct e_e_struct = null;
  public E_union e_e_union = null;
  public B[] e_e_sequence = null;
  public B[] e_e_array = null;

  public G_struct()
  {
  } // ctor

  public G_struct(E_struct _e_e_struct, E_union _e_e_union, B[] _e_e_sequence,
                  B[] _e_e_array
                 )
  {
    e_e_struct = _e_e_struct;
    e_e_union = _e_e_union;
    e_e_sequence = _e_e_sequence;
    e_e_array = _e_e_array;
  } // ctor
} // class G_struct
