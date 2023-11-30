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


// F
package gnu.testlet.org.omg.CORBA.ORB.RF11;

public final class F_struct
  implements org.omg.CORBA.portable.IDLEntity
{
  public C_struct e_c_struct = null;
  public C_union e_c_union = null;
  public short[] e_c_sequence_e_short = null;
  public short[] e_c_sequence_e_ushort = null;
  public int[] e_c_sequence_e_long = null;
  public int[] e_c_sequence_e_ulong = null;
  public float[] e_c_sequence_e_float = null;
  public double[] e_c_sequence_e_double = null;
  public char[] e_c_sequence_e_char = null;
  public boolean[] e_c_sequence_e_boolean = null;
  public byte[] e_c_sequence_e_octet = null;
  public org.omg.CORBA.Any[] e_c_sequence_e_any = null;
  public String[] e_c_sequence_e_string = null;
  public org.omg.CORBA.Object[] e_c_sequence_e_Object = null;
  public short[] e_c_array_e_short = null;
  public short[] e_c_array_e_ushort = null;
  public int[] e_c_array_e_long = null;
  public int[] e_c_array_e_ulong = null;
  public float[] e_c_array_e_float = null;
  public double[] e_c_array_e_double = null;
  public char[] e_c_array_e_char = null;
  public boolean[] e_c_array_e_boolean = null;
  public byte[] e_c_array_e_octet = null;
  public org.omg.CORBA.Any[] e_c_array_e_any = null;
  public String[] e_c_array_e_string = null;
  public org.omg.CORBA.Object[] e_c_array_e_Object = null;

  public F_struct()
  {
  } // ctor

  public F_struct(C_struct _e_c_struct, C_union _e_c_union,
                  short[] _e_c_sequence_e_short,
                  short[] _e_c_sequence_e_ushort, int[] _e_c_sequence_e_long,
                  int[] _e_c_sequence_e_ulong, float[] _e_c_sequence_e_float,
                  double[] _e_c_sequence_e_double, char[] _e_c_sequence_e_char,
                  boolean[] _e_c_sequence_e_boolean,
                  byte[] _e_c_sequence_e_octet,
                  org.omg.CORBA.Any[] _e_c_sequence_e_any,
                  String[] _e_c_sequence_e_string,
                  org.omg.CORBA.Object[] _e_c_sequence_e_Object,
                  short[] _e_c_array_e_short, short[] _e_c_array_e_ushort,
                  int[] _e_c_array_e_long, int[] _e_c_array_e_ulong,
                  float[] _e_c_array_e_float, double[] _e_c_array_e_double,
                  char[] _e_c_array_e_char, boolean[] _e_c_array_e_boolean,
                  byte[] _e_c_array_e_octet,
                  org.omg.CORBA.Any[] _e_c_array_e_any,
                  String[] _e_c_array_e_string,
                  org.omg.CORBA.Object[] _e_c_array_e_Object
                 )
  {
    e_c_struct = _e_c_struct;
    e_c_union = _e_c_union;
    e_c_sequence_e_short = _e_c_sequence_e_short;
    e_c_sequence_e_ushort = _e_c_sequence_e_ushort;
    e_c_sequence_e_long = _e_c_sequence_e_long;
    e_c_sequence_e_ulong = _e_c_sequence_e_ulong;
    e_c_sequence_e_float = _e_c_sequence_e_float;
    e_c_sequence_e_double = _e_c_sequence_e_double;
    e_c_sequence_e_char = _e_c_sequence_e_char;
    e_c_sequence_e_boolean = _e_c_sequence_e_boolean;
    e_c_sequence_e_octet = _e_c_sequence_e_octet;
    e_c_sequence_e_any = _e_c_sequence_e_any;
    e_c_sequence_e_string = _e_c_sequence_e_string;
    e_c_sequence_e_Object = _e_c_sequence_e_Object;
    e_c_array_e_short = _e_c_array_e_short;
    e_c_array_e_ushort = _e_c_array_e_ushort;
    e_c_array_e_long = _e_c_array_e_long;
    e_c_array_e_ulong = _e_c_array_e_ulong;
    e_c_array_e_float = _e_c_array_e_float;
    e_c_array_e_double = _e_c_array_e_double;
    e_c_array_e_char = _e_c_array_e_char;
    e_c_array_e_boolean = _e_c_array_e_boolean;
    e_c_array_e_octet = _e_c_array_e_octet;
    e_c_array_e_any = _e_c_array_e_any;
    e_c_array_e_string = _e_c_array_e_string;
    e_c_array_e_Object = _e_c_array_e_Object;
  } // ctor
} // class F_struct
