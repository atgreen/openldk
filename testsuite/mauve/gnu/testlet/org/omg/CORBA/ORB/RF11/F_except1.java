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

public final class F_except1
  extends org.omg.CORBA.UserException
  implements org.omg.CORBA.portable.IDLEntity
{
  public F_struct v1 = null;
  public F_union v2 = null;

  // C_struct
  public F_union v3 = null;

  // C_union
  public F_union v4 = null;

  // C_sequence_e_short
  public F_union v5 = null;

  // C_sequence_e_ushort
  public F_union v6 = null;

  // C_sequence_e_long
  public F_union v7 = null;

  // C_sequence_e_ulong
  public F_union v8 = null;

  // C_sequence e_float
  public F_union v9 = null;

  // C_sequence_e_double
  public F_union v10 = null;

  // C_sequence_e_char
  public F_union v11 = null;

  // C_sequence_e_boolean
  public F_union v12 = null;

  // C_sequence_e_octet
  public F_union v13 = null;

  // C_sequence_e_any
  public F_union v14 = null;

  // C_sequence_e_string
  public F_union v15 = null;

  // C_sequence_e_Object
  public F_union v18 = null;

  // C_array_e_short
  public F_union v19 = null;

  // C_array_e_ushort
  public F_union v20 = null;

  // C_array_e_long
  public F_union v21 = null;

  // C_array_e_ulong
  public F_union v22 = null;

  // C_array e_float
  public F_union v23 = null;

  // C_array_e_double
  public F_union v24 = null;

  // C_array_e_char
  public F_union v25 = null;

  // C_array_e_boolean
  public F_union v26 = null;

  // C_array_e_octet
  public F_union v27 = null;

  // C_array_e_any
  public F_union v28 = null;

  // C_array_e_string
  public F_union v29 = null;

  public F_except1()
  {
  } // ctor

  public F_except1(F_struct _v1, F_union _v2, F_union _v3, F_union _v4,
                   F_union _v5, F_union _v6, F_union _v7, F_union _v8,
                   F_union _v9, F_union _v10, F_union _v11, F_union _v12,
                   F_union _v13, F_union _v14, F_union _v15, F_union _v18,
                   F_union _v19, F_union _v20, F_union _v21, F_union _v22,
                   F_union _v23, F_union _v24, F_union _v25, F_union _v26,
                   F_union _v27, F_union _v28, F_union _v29
                  )
  {
    v1 = _v1;
    v2 = _v2;
    v3 = _v3;
    v4 = _v4;
    v5 = _v5;
    v6 = _v6;
    v7 = _v7;
    v8 = _v8;
    v9 = _v9;
    v10 = _v10;
    v11 = _v11;
    v12 = _v12;
    v13 = _v13;
    v14 = _v14;
    v15 = _v15;
    v18 = _v18;
    v19 = _v19;
    v20 = _v20;
    v21 = _v21;
    v22 = _v22;
    v23 = _v23;
    v24 = _v24;
    v25 = _v25;
    v26 = _v26;
    v27 = _v27;
    v28 = _v28;
    v29 = _v29;
  } // ctor
} // class F_except1
