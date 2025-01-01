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

public final class C_except
  extends org.omg.CORBA.UserException
  implements org.omg.CORBA.portable.IDLEntity
{
  public C_struct v1 = null;
  public C_union v2 = null;

  // short
  public C_union v3 = null;

  // ushort
  public C_union v4 = null;

  // long
  public C_union v5 = null;

  // ulong
  public C_union v6 = null;

  // float
  public C_union v7 = null;

  // double
  public C_union v8 = null;

  // char
  public C_union v9 = null;

  // boolean
  public C_union v10 = null;

  // octet
  public C_union v11 = null;

  // any
  public C_union v12 = null;

  // string
  public C_union v13 = null;

  // Object
  public short[] v16 = null;
  public short[] v17 = null;
  public int[] v18 = null;
  public int[] v19 = null;
  public float[] v20 = null;
  public double[] v21 = null;
  public char[] v22 = null;
  public boolean[] v23 = null;
  public byte[] v24 = null;
  public org.omg.CORBA.Any[] v25 = null;
  public String[] v26 = null;
  public org.omg.CORBA.Object[] v27 = null;
  public short[] v30 = null;
  public short[] v31 = null;
  public int[] v32 = null;
  public int[] v33 = null;
  public float[] v34 = null;
  public double[] v35 = null;
  public char[] v36 = null;
  public boolean[] v37 = null;
  public byte[] v38 = null;
  public org.omg.CORBA.Any[] v39 = null;
  public String[] v40 = null;
  public org.omg.CORBA.Object[] v41 = null;

  public C_except()
  {
  } // ctor

  public C_except(C_struct _v1, C_union _v2, C_union _v3, C_union _v4,
                  C_union _v5, C_union _v6, C_union _v7, C_union _v8,
                  C_union _v9, C_union _v10, C_union _v11, C_union _v12,
                  C_union _v13, short[] _v16, short[] _v17, int[] _v18,
                  int[] _v19, float[] _v20, double[] _v21, char[] _v22,
                  boolean[] _v23, byte[] _v24, org.omg.CORBA.Any[] _v25,
                  String[] _v26, org.omg.CORBA.Object[] _v27, short[] _v30,
                  short[] _v31, int[] _v32, int[] _v33, float[] _v34,
                  double[] _v35, char[] _v36, boolean[] _v37, byte[] _v38,
                  org.omg.CORBA.Any[] _v39, String[] _v40,
                  org.omg.CORBA.Object[] _v41
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
    v16 = _v16;
    v17 = _v17;
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
    v30 = _v30;
    v31 = _v31;
    v32 = _v32;
    v33 = _v33;
    v34 = _v34;
    v35 = _v35;
    v36 = _v36;
    v37 = _v37;
    v38 = _v38;
    v39 = _v39;
    v40 = _v40;
    v41 = _v41;
  } // ctor
} // class C_except
