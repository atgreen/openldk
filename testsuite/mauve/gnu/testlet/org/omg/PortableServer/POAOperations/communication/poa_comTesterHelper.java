// Tags: not-a-test
// Uses: poa_comTesterStub

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

import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.ObjectImpl;

/**
 * The tester interface.
 */
public abstract class poa_comTesterHelper
{
  private static String _id =
    "IDL:gnu/testlet/org/omg/PortableServer/POAOperations/communication/comTester:1.0";

  public static synchronized org.omg.CORBA.TypeCode type()
  {
    return org.omg.CORBA.ORB.init().create_interface_tc(poa_comTesterHelper.id(),
                                                        "comTester"
                                                       );
  }

  public static String id()
  {
    return _id;
  }

  public static poa_comTester narrow(org.omg.CORBA.Object obj)
  {
    if (obj == null)
      return null;
    else if (obj instanceof poa_comTester)
      return (poa_comTester) obj;
    else if (!obj._is_a(id()))
      throw new org.omg.CORBA.BAD_PARAM();
    else
      {
        Delegate delegate = ((ObjectImpl) obj)._get_delegate();
        poa_comTesterStub stub = new poa_comTesterStub();
        stub._set_delegate(delegate);
        return stub;
      }
  }
}
