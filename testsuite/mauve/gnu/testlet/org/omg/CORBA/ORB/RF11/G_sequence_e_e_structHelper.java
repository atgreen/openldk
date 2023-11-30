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

public abstract class G_sequence_e_e_structHelper
{
  private static String _id = "IDL:gnu/testlet/org/omg/CORBA/ORB/RF11/G_sequence_e_e_struct:1.0";

  public static void insert(org.omg.CORBA.Any a, E_struct[] that)
  {
    org.omg.CORBA.portable.OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static E_struct[] extract(org.omg.CORBA.Any a)
  {
    return read(a.create_input_stream());
  }

  private static org.omg.CORBA.TypeCode __typeCode = null;

  public static synchronized org.omg.CORBA.TypeCode type()
  {
    if (__typeCode == null)
      {
        __typeCode = E_structHelper.type();
        __typeCode = org.omg.CORBA.ORB.init().create_sequence_tc(0, __typeCode);
        __typeCode =
          org.omg.CORBA.ORB.init().create_alias_tc(G_sequence_e_e_structHelper.id(),
                                                   "G_sequence_e_e_struct",
                                                   __typeCode
                                                  );
      }
    return __typeCode;
  }

  public static String id()
  {
    return _id;
  }

  public static E_struct[] read(org.omg.CORBA.portable.InputStream istream)
  {
    E_struct[] value = null;
    int _len0 = istream.read_long();
    value = new E_struct[ _len0 ];
    for (int _o1 = 0; _o1 < value.length; ++_o1)
      value [ _o1 ] = E_structHelper.read(istream);
    return value;
  }

  public static void write(org.omg.CORBA.portable.OutputStream ostream,
                           E_struct[] value
                          )
  {
    ostream.write_long(value.length);
    for (int _i0 = 0; _i0 < value.length; ++_i0)
      E_structHelper.write(ostream, value [ _i0 ]);
  }
}
