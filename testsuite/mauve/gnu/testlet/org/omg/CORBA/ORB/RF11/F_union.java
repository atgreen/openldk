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

public final class F_union
  implements org.omg.CORBA.portable.IDLEntity
{
  private C_struct ___e_c_struct;
  private C_union ___e_c_union;
  private short[] ___e_c_sequence_e_short;
  private short[] ___e_c_sequence_e_ushort;
  private int[] ___e_c_sequence_e_long;
  private int[] ___e_c_sequence_e_ulong;
  private float[] ___e_c_sequence_e_float;
  private double[] ___e_c_sequence_e_double;
  private char[] ___e_c_sequence_e_char;
  private boolean[] ___e_c_sequence_e_boolean;
  private byte[] ___e_c_sequence_e_octet;
  private org.omg.CORBA.Any[] ___e_c_sequence_e_any;
  private String[] ___e_c_sequence_e_string;
  private org.omg.CORBA.Object[] ___e_c_sequence_e_Object;
  private short[] ___e_c_array_e_short;
  private short[] ___e_c_array_e_ushort;
  private int[] ___e_c_array_e_long;
  private int[] ___e_c_array_e_ulong;
  private float[] ___e_c_array_e_float;
  private double[] ___e_c_array_e_double;
  private char[] ___e_c_array_e_char;
  private boolean[] ___e_c_array_e_boolean;
  private byte[] ___e_c_array_e_octet;
  private org.omg.CORBA.Any[] ___e_c_array_e_any;
  private String[] ___e_c_array_e_string;
  private org.omg.CORBA.Object[] ___e_c_array_e_Object;
  private int __discriminator;
  private boolean __uninitialized = true;

  public F_union()
  {
  }

  public int discriminator()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    return __discriminator;
  }

  public C_struct e_c_struct()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_struct(__discriminator);
    return ___e_c_struct;
  }

  public void e_c_struct(C_struct value)
  {
    __discriminator = 1;
    ___e_c_struct = value;
    __uninitialized = false;
  }

  private void verifye_c_struct(int discriminator)
  {
    if (discriminator != 1)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public C_union e_c_union()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_union(__discriminator);
    return ___e_c_union;
  }

  public void e_c_union(C_union value)
  {
    __discriminator = 2;
    ___e_c_union = value;
    __uninitialized = false;
  }

  private void verifye_c_union(int discriminator)
  {
    if (discriminator != 2)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public short[] e_c_sequence_e_short()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_short(__discriminator);
    return ___e_c_sequence_e_short;
  }

  public void e_c_sequence_e_short(short[] value)
  {
    __discriminator = 3;
    ___e_c_sequence_e_short = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_short(int discriminator)
  {
    if (discriminator != 3)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public short[] e_c_sequence_e_ushort()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_ushort(__discriminator);
    return ___e_c_sequence_e_ushort;
  }

  public void e_c_sequence_e_ushort(short[] value)
  {
    __discriminator = 4;
    ___e_c_sequence_e_ushort = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_ushort(int discriminator)
  {
    if (discriminator != 4)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public int[] e_c_sequence_e_long()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_long(__discriminator);
    return ___e_c_sequence_e_long;
  }

  public void e_c_sequence_e_long(int[] value)
  {
    __discriminator = 5;
    ___e_c_sequence_e_long = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_long(int discriminator)
  {
    if (discriminator != 5)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public int[] e_c_sequence_e_ulong()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_ulong(__discriminator);
    return ___e_c_sequence_e_ulong;
  }

  public void e_c_sequence_e_ulong(int[] value)
  {
    __discriminator = 6;
    ___e_c_sequence_e_ulong = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_ulong(int discriminator)
  {
    if (discriminator != 6)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public float[] e_c_sequence_e_float()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_float(__discriminator);
    return ___e_c_sequence_e_float;
  }

  public void e_c_sequence_e_float(float[] value)
  {
    __discriminator = 7;
    ___e_c_sequence_e_float = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_float(int discriminator)
  {
    if (discriminator != 7)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public double[] e_c_sequence_e_double()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_double(__discriminator);
    return ___e_c_sequence_e_double;
  }

  public void e_c_sequence_e_double(double[] value)
  {
    __discriminator = 8;
    ___e_c_sequence_e_double = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_double(int discriminator)
  {
    if (discriminator != 8)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public char[] e_c_sequence_e_char()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_char(__discriminator);
    return ___e_c_sequence_e_char;
  }

  public void e_c_sequence_e_char(char[] value)
  {
    __discriminator = 9;
    ___e_c_sequence_e_char = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_char(int discriminator)
  {
    if (discriminator != 9)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public boolean[] e_c_sequence_e_boolean()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_boolean(__discriminator);
    return ___e_c_sequence_e_boolean;
  }

  public void e_c_sequence_e_boolean(boolean[] value)
  {
    __discriminator = 10;
    ___e_c_sequence_e_boolean = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_boolean(int discriminator)
  {
    if (discriminator != 10)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public byte[] e_c_sequence_e_octet()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_octet(__discriminator);
    return ___e_c_sequence_e_octet;
  }

  public void e_c_sequence_e_octet(byte[] value)
  {
    __discriminator = 11;
    ___e_c_sequence_e_octet = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_octet(int discriminator)
  {
    if (discriminator != 11)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public org.omg.CORBA.Any[] e_c_sequence_e_any()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_any(__discriminator);
    return ___e_c_sequence_e_any;
  }

  public void e_c_sequence_e_any(org.omg.CORBA.Any[] value)
  {
    __discriminator = 12;
    ___e_c_sequence_e_any = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_any(int discriminator)
  {
    if (discriminator != 12)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public String[] e_c_sequence_e_string()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_string(__discriminator);
    return ___e_c_sequence_e_string;
  }

  public void e_c_sequence_e_string(String[] value)
  {
    __discriminator = 13;
    ___e_c_sequence_e_string = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_string(int discriminator)
  {
    if (discriminator != 13)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public org.omg.CORBA.Object[] e_c_sequence_e_Object()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_sequence_e_Object(__discriminator);
    return ___e_c_sequence_e_Object;
  }

  public void e_c_sequence_e_Object(org.omg.CORBA.Object[] value)
  {
    __discriminator = 14;
    ___e_c_sequence_e_Object = value;
    __uninitialized = false;
  }

  private void verifye_c_sequence_e_Object(int discriminator)
  {
    if (discriminator != 14)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public short[] e_c_array_e_short()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_short(__discriminator);
    return ___e_c_array_e_short;
  }

  public void e_c_array_e_short(short[] value)
  {
    __discriminator = 17;
    ___e_c_array_e_short = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_short(int discriminator)
  {
    if (discriminator != 17)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public short[] e_c_array_e_ushort()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_ushort(__discriminator);
    return ___e_c_array_e_ushort;
  }

  public void e_c_array_e_ushort(short[] value)
  {
    __discriminator = 18;
    ___e_c_array_e_ushort = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_ushort(int discriminator)
  {
    if (discriminator != 18)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public int[] e_c_array_e_long()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_long(__discriminator);
    return ___e_c_array_e_long;
  }

  public void e_c_array_e_long(int[] value)
  {
    __discriminator = 19;
    ___e_c_array_e_long = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_long(int discriminator)
  {
    if (discriminator != 19)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public int[] e_c_array_e_ulong()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_ulong(__discriminator);
    return ___e_c_array_e_ulong;
  }

  public void e_c_array_e_ulong(int[] value)
  {
    __discriminator = 20;
    ___e_c_array_e_ulong = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_ulong(int discriminator)
  {
    if (discriminator != 20)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public float[] e_c_array_e_float()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_float(__discriminator);
    return ___e_c_array_e_float;
  }

  public void e_c_array_e_float(float[] value)
  {
    __discriminator = 21;
    ___e_c_array_e_float = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_float(int discriminator)
  {
    if (discriminator != 21)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public double[] e_c_array_e_double()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_double(__discriminator);
    return ___e_c_array_e_double;
  }

  public void e_c_array_e_double(double[] value)
  {
    __discriminator = 22;
    ___e_c_array_e_double = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_double(int discriminator)
  {
    if (discriminator != 22)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public char[] e_c_array_e_char()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_char(__discriminator);
    return ___e_c_array_e_char;
  }

  public void e_c_array_e_char(char[] value)
  {
    __discriminator = 23;
    ___e_c_array_e_char = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_char(int discriminator)
  {
    if (discriminator != 23)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public boolean[] e_c_array_e_boolean()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_boolean(__discriminator);
    return ___e_c_array_e_boolean;
  }

  public void e_c_array_e_boolean(boolean[] value)
  {
    __discriminator = 24;
    ___e_c_array_e_boolean = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_boolean(int discriminator)
  {
    if (discriminator != 24)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public byte[] e_c_array_e_octet()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_octet(__discriminator);
    return ___e_c_array_e_octet;
  }

  public void e_c_array_e_octet(byte[] value)
  {
    __discriminator = 25;
    ___e_c_array_e_octet = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_octet(int discriminator)
  {
    if (discriminator != 25)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public org.omg.CORBA.Any[] e_c_array_e_any()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_any(__discriminator);
    return ___e_c_array_e_any;
  }

  public void e_c_array_e_any(org.omg.CORBA.Any[] value)
  {
    __discriminator = 26;
    ___e_c_array_e_any = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_any(int discriminator)
  {
    if (discriminator != 26)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public String[] e_c_array_e_string()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_string(__discriminator);
    return ___e_c_array_e_string;
  }

  public void e_c_array_e_string(String[] value)
  {
    __discriminator = 27;
    ___e_c_array_e_string = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_string(int discriminator)
  {
    if (discriminator != 27)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public org.omg.CORBA.Object[] e_c_array_e_Object()
  {
    if (__uninitialized)
      throw new org.omg.CORBA.BAD_OPERATION();
    verifye_c_array_e_Object(__discriminator);
    return ___e_c_array_e_Object;
  }

  public void e_c_array_e_Object(org.omg.CORBA.Object[] value)
  {
    __discriminator = 28;
    ___e_c_array_e_Object = value;
    __uninitialized = false;
  }

  private void verifye_c_array_e_Object(int discriminator)
  {
    if (discriminator != 28)
      throw new org.omg.CORBA.BAD_OPERATION();
  }

  public void _default()
  {
    __discriminator = -2147483648;
    __uninitialized = false;
  }
} // class F_union
