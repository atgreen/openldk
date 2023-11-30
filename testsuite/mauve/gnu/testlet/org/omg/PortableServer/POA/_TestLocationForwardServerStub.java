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

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.ORB;
import org.omg.CORBA.ObjectHelper;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

public class _TestLocationForwardServerStub
  extends ObjectImpl
  implements TestLocationForwardServer
{
  public void setForwardRequest(org.omg.CORBA.Object obj)
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("setForwardRequest", true);
        ObjectHelper.write(out, obj);
        in = _invoke(out);
        return;
      }
    catch (ApplicationException $ex)
      {
        in = $ex.getInputStream();

        String _id = $ex.getId();
        throw new MARSHAL(_id);
      }
    catch (RemarshalException $rm)
      {
        setForwardRequest(obj);
      }
    finally
      {
        _releaseReply(in);
      }
  } // setForwardRequest

  public org.omg.CORBA.Object get_servant()
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("get_servant", true);
        in = _invoke(out);

        return ObjectHelper.read(in);
      }
    catch (ApplicationException $ex)
      {
        in = $ex.getInputStream();

        String _id = $ex.getId();
        throw new MARSHAL(_id);
      }
    catch (RemarshalException $rm)
      {
        return get_servant();
      }
    finally
      {
        _releaseReply(in);
      }
  } // get_servant

  public void deactivate()
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("deactivate", true);
        in = _invoke(out);
        return;
      }
    catch (ApplicationException $ex)
      {
        in = $ex.getInputStream();

        String _id = $ex.getId();
        throw new MARSHAL(_id);
      }
    catch (RemarshalException $rm)
      {
        deactivate();
      }
    finally
      {
        _releaseReply(in);
      }
  } // deactivate

  // Type-specific CORBA::Object operations
  private static String[] __ids =
    { "IDL:test/poa/TestLocationForwardServer:1.0" };

  public String[] _ids()
  {
    return __ids;
  }

  private void readObject(java.io.ObjectInputStream s)
                   throws java.io.IOException
  {
    String str = s.readUTF();
    String[] args = null;
    java.util.Properties props = null;
    org.omg.CORBA.Object obj = ORB.init(args, props).string_to_object(str);
    Delegate delegate = ((ObjectImpl) obj)._get_delegate();
    _set_delegate(delegate);
  }

  private void writeObject(java.io.ObjectOutputStream s)
                    throws java.io.IOException
  {
    String[] args = null;
    java.util.Properties props = null;
    String str = ORB.init(args, props).object_to_string(this);
    s.writeUTF(str);
  }
} // class _TestLocationForwardServerStub
