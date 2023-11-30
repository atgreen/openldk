// Tags: not-a-test
// Copyright (C) 2005 Audrius Meskauskas (AudriusA@Bioinformatics.org)

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
// Boston, MA 02111-1307, USA.  */


package gnu.testlet.org.omg.PortableServer.POAOperations.communication;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

public abstract class ourUserExceptionHelper
{
  private static String the_id =
    "IDL:gnu/testlet/org/omg/PortableServer/POAOperations/communication/ourUserException:1.0";

  public static void insert(Any a, ourUserException that)
  {
    OutputStream out = a.create_output_stream();
    a.type(type());
    write(out, that);
    a.read_value(out.create_input_stream(), type());
  }

  public static ourUserException extract(Any a)
  {
    return read(a.create_input_stream());
  }

  public static synchronized TypeCode type()
  {
    StructMember[] members = new StructMember[ 1 ];
    TypeCode member = ORB.init().get_primitive_tc(TCKind.tk_long);
    members [ 0 ] = new StructMember("ourField", member, null);
    return ORB.init().create_exception_tc(ourUserExceptionHelper.id(),
                                          "ourUserException", members
                                         );
  }

  public static String id()
  {
    return the_id;
  }

  public static ourUserException read(InputStream istream)
  {
    ourUserException value = new ourUserException();

    // read and discard the repository ID
    istream.read_string();
    value.ourField = istream.read_long();
    return value;
  }

  public static void write(OutputStream ostream, ourUserException value)
  {
    // write the repository ID
    ostream.write_string(id());
    ostream.write_long(value.ourField);
  }
}
