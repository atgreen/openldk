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

public final class A_except1
  extends org.omg.CORBA.UserException
  implements org.omg.CORBA.portable.IDLEntity
{
  public short v1 = (short) 0;
  public short v2 = (short) 0;
  public int v3 = (int) 0;
  public int v4 = (int) 0;
  public float v5 = (float) 0;
  public double v6 = (double) 0;
  public char v7 = (char) 0;
  public boolean v8 = false;
  public byte v9 = (byte) 0;

  public A_except1()
  {
  } // ctor

  public A_except1(short _v1, short _v2, int _v3, int _v4, float _v5,
                   double _v6, char _v7, boolean _v8, byte _v9
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
  } // ctor
} // class A_except1
