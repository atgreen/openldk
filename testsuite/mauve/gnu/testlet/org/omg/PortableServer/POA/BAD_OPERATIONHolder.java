// Copyright (c) Object Oriented Concepts, Inc. Billerica, MA, USA

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

package gnu.testlet.org.omg.PortableServer.POA;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.CompletionStatusHelper;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

public class BAD_OPERATIONHolder
  implements Streamable
{
  public BAD_OPERATION value;
  String id = "IDL:omg.org/CORBA/BAD_OPERATION:1.0";

  public BAD_OPERATIONHolder(BAD_OPERATION v)
  {
    value = v;
  }

  public BAD_OPERATIONHolder()
  {
  }
  ;
  public TypeCode _type()
  {
    return ORB.init().create_exception_tc(id, "BAD_OPERATION",
                                          new StructMember[ 0 ]
                                         );
  }

  public void _write(OutputStream output)
  {
    output.write_string(id);
    output.write_long(value.minor);
    CompletionStatusHelper.write(output, value.completed);
  }

  public void _read(InputStream input)
  {
    String ri = input.read_string();
    int minor = input.read_long();

    if (!ri.equals(id))
      throw new RuntimeException("Id value mismath: '" + ri + "' expected '" +
                                 id + "'"
                                );

    CompletionStatus status = CompletionStatusHelper.read(input);
    value = new BAD_OPERATION(minor, status);
  }
}