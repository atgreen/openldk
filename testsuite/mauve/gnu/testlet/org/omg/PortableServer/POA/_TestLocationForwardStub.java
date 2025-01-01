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

public class _TestLocationForwardStub
  extends org.omg.CORBA.portable.ObjectImpl
  implements TestLocationForward
{
  public void deactivate_servant()
  {
    org.omg.CORBA.portable.InputStream in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream out =
          _request("deactivate_servant", true);
        in = _invoke(out);
        return;
      }
    catch (org.omg.CORBA.portable.ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException $rm)
      {
        deactivate_servant();
      }
    finally
      {
        _releaseReply(in);
      }
  } // deactivate_servant

  public void aMethod()
  {
    org.omg.CORBA.portable.InputStream in = null;
    try
      {
        org.omg.CORBA.portable.OutputStream out = _request("aMethod", true);
        in = _invoke(out);
        return;
      }
    catch (org.omg.CORBA.portable.ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new org.omg.CORBA.MARSHAL(_id);
      }
    catch (org.omg.CORBA.portable.RemarshalException $rm)
      {
        aMethod();
      }
    finally
      {
        _releaseReply(in);
      }
  } // aMethod

  // Type-specific CORBA::Object operations
  private static String[] __ids =
    { "IDL:test/poa/TestLocationForward:1.0", "IDL:test/poa/Test:1.0" };

  public String[] _ids()
  {
    return __ids;
  }
} // class _TestLocationForwardStub
