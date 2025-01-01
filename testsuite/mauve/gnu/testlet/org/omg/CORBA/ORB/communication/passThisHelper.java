// Tags: not-a-test
/* passThisHelper.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */



package gnu.testlet.org.omg.CORBA.ORB.communication;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the {@link passThis}.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class passThisHelper
{
  /**
   * The repository ID of the {@link passThis}.
   */
  private static String id =
    "IDL:gnu/testlet/org/omg/CORBA/ORB/communication/passThis:1.0";

  /**
   * Get the repository id.
   */
  public static String id()
  {
    return id;
  }

  /**
   * Read the structure from the CDR stram.
   */
  public static passThis read(InputStream istream)
  {
    passThis value = new passThis();
    value.a = istream.read_string();
    value.b = istream.read_wstring();
    return value;
  }

  /**
   * Get the type code of this structure.
   */
  public static synchronized TypeCode type()
  {
    StructMember[] members = new StructMember[ 2 ];
    TypeCode member = null;
    member = ORB.init().create_string_tc(0);
    members [ 0 ] = new StructMember("a", member, null);
    member = ORB.init().create_string_tc(0);
    members [ 1 ] = new StructMember("b", member, null);
    return ORB.init().create_struct_tc(passThisHelper.id(), "passThis", members);
  }

  /**
   * Write the structure into the CDR stream.
   */
  public static void write(OutputStream ostream, passThis value)
  {
    ostream.write_string(value.a);
    ostream.write_wstring(value.b);
  }
}
