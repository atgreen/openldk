// Tags: not-a-test
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

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.PortableServer.Servant;

import java.util.Hashtable;

public abstract class TestPOA
  extends org.omg.PortableServer.Servant
  implements TestOperations, InvokeHandler
{
  // Constructors
  private static Hashtable _methods = new Hashtable();

  static
  {
    _methods.put("aMethod", new java.lang.Integer(0));
  }

  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    OutputStream out = null;
    Integer __method = (Integer) _methods.get(method);
    if (__method == null)
      throw new org.omg.CORBA.BAD_OPERATION(0,
                                            org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE
                                           );

    switch (__method.intValue())
      {
        case 0 : // test/poa/Test/aMethod
        {
          this.aMethod();
          out = rh.createReply();
          break;
        }

        default :
          throw new org.omg.CORBA.BAD_OPERATION(0,
                                                org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE
                                               );
      }

    return out;
  } // _invoke

  // Type-specific CORBA::Object operations
  private static String[] __ids = { "IDL:test/poa/Test:1.0" };

  public String[] _all_interfaces(org.omg.PortableServer.POA poa,
                                  byte[] objectId
                                 )
  {
    return __ids;
  }

  public Test _this()
  {
    return TestHelper.narrow(super._this_object());
  }

  public Test _this(org.omg.CORBA.ORB orb)
  {
    return TestHelper.narrow(super._this_object(orb));
  }
}
