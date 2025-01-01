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

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;

/**
 * The remote POA control interface.
 */
public abstract class remotePoaControlHelper
{
  private static String _id =
    "IDL:gnu/testlet/org/omg/PortableServer/POAOperations/communication/remotePoaControl:1.0";

  public static synchronized TypeCode type()
  {
    return ORB.init().create_interface_tc(remotePoaControlHelper.id(),
                                          "remotePoaControl"
                                         );
  }

  public static String id()
  {
    return _id;
  }

  public static remotePoaControl narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof remotePoaControl)
      return (remotePoaControl) obj;
    else if (!obj._is_a(id()))
      throw new BAD_PARAM();
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        poa_remotePoaControlStub stub = new poa_remotePoaControlStub();
        stub._set_delegate(delegate);
        return stub;
      }
  }
}
