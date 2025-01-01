// Not a test, required by RMI_IIOP.java.

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

package gnu.testlet.javax.rmi.CORBA.Tie;

import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.Info;
import gnu.testlet.org.omg.CORBA_2_3.ORB.Valtype.cmInfo;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.rmi.UnexpectedException;
import java.util.Collection;

import javax.rmi.PortableRemoteObject;
import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.Util;

import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;
import org.omg.CORBA.portable.ServantObject;

/**
 * This file is part of the CORBA RMI over IIOP the test executable
 * class being gnu.testlet.javax.rmi.CORBA.Tie.RMI_IIOP.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class _RMI_test_Stub
  extends Stub
  implements RMI_test
{

  private static final String[] _type_ids = _RMI_testImpl_Tie._type_ids;

  public String[] _ids()
  {
    return _type_ids;
  }

  public String sayHello(RMI_test arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("sayHello", true);
                Util.writeRemoteObject(out, arg0);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return sayHello(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("sayHello", RMI_test.class);
        if (so == null)
          {
            return sayHello(arg0);
          }
        try
          {
            RMI_test arg0Copy = (RMI_test) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).sayHello(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String joinStrings(String arg0, String arg1)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "joinStrings", true);
                out.write_value(arg0, String.class);
                out.write_value(arg1, String.class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return joinStrings(arg0, arg1);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("joinStrings", RMI_test.class);
        if (so == null)
          {
            return joinStrings(arg0, arg1);
          }
        try
          {
            return ((RMI_test) so.servant).joinStrings(arg0, arg1);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public long multiply(byte arg0, long arg1)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("multiply", true);
                out.write_octet(arg0);
                out.write_longlong(arg1);
                in = _invoke(out);
                return in.read_longlong();
              }
            catch (ApplicationException ex)
              {
                in = ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return multiply(arg0, arg1);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("multiply", RMI_test.class);
        if (so == null)
          {
            return multiply(arg0, arg1);
          }
        try
          {
            return ((RMI_test) so.servant).multiply(arg0, arg1);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public int passArray(int[] arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passArray", true);
                out.write_value(cast_array(arg0), int[].class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return in.read_long();
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passArray(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passArray", RMI_test.class);
        if (so == null)
          {
            return passArray(arg0);
          }
        try
          {
            int[] arg0Copy = (int[]) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passArray(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passStringArray(String[] arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passStringArray", true);
                out.write_value(cast_array(arg0), String[].class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passStringArray(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passStringArray", RMI_test.class);
        if (so == null)
          {
            return passStringArray(arg0);
          }
        try
          {
            String[] arg0Copy = (String[]) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passStringArray(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passPrimitives(byte arg0, double arg1, int arg2, String arg3,
    float arg4, char arg5, short arg6)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passPrimitives", true);
                out.write_octet(arg0);
                out.write_double(arg1);
                out.write_long(arg2);
                out.write_value(arg3, String.class);
                out.write_float(arg4);
                out.write_wchar(arg5);
                out.write_short(arg6);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passPrimitives(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passPrimitives", RMI_test.class);
        if (so == null)
          {
            return passPrimitives(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
          }
        try
          {
            return ((RMI_test) so.servant).passPrimitives(arg0, arg1, arg2,
              arg3, arg4, arg5, arg6);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passStructure(myStructure arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passStructure", true);
                out.write_value(arg0, myStructure.class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passStructure(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passStructure", RMI_test.class);
        if (so == null)
          {
            return passStructure(arg0);
          }
        try
          {
            myStructure arg0Copy = (myStructure) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passStructure(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passStructureArray(myStructure[] arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passStructureArray", true);
                out.write_value(cast_array(arg0), myStructure[].class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passStructureArray(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passStructureArray",
          RMI_test.class);
        if (so == null)
          {
            return passStructureArray(arg0);
          }
        try
          {
            myStructure[] arg0Copy = (myStructure[]) Util.copyObject(arg0,
              _orb());
            return ((RMI_test) so.servant).passStructureArray(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passCorbaCMValueType(cmInfo arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passCorbaCMValueType", true);
                out.write_value(arg0, cmInfo.class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passCorbaCMValueType(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passCorbaCMValueType",
          RMI_test.class);
        if (so == null)
          {
            return passCorbaCMValueType(arg0);
          }
        try
          {
            cmInfo arg0Copy = (cmInfo) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passCorbaCMValueType(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passCorbaValueType(Info arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passCorbaValueType", true);
                out.write_value(arg0, Info.class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passCorbaValueType(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passCorbaValueType",
          RMI_test.class);
        if (so == null)
          {
            return passCorbaValueType(arg0);
          }
        try
          {
            Info arg0Copy = (Info) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passCorbaValueType(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passCorbaValueTypeArray(Info[] arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passCorbaValueTypeArray", true);
                out.write_value(cast_array(arg0), Info[].class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passCorbaValueTypeArray(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passCorbaValueTypeArray",
          RMI_test.class);
        if (so == null)
          {
            return passCorbaValueTypeArray(arg0);
          }
        try
          {
            Info[] arg0Copy = (Info[]) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passCorbaValueTypeArray(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passCorbaObject(org.omg.CORBA.Object arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("passCorbaObject", true);
                out.write_Object(arg0);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passCorbaObject(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passCorbaObject", RMI_test.class);
        if (so == null)
          {
            return passCorbaObject(arg0);
          }
        try
          {
            return ((RMI_test) so.servant).passCorbaObject(arg0);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public NodeObject exchangeNodeObject(NodeObject arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "exchangeNodeObject", true);
                out.write_value(arg0, NodeObject.class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (NodeObject) in.read_value(NodeObject.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return exchangeNodeObject(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("exchangeNodeObject",
          RMI_test.class);
        if (so == null)
          {
            return exchangeNodeObject(arg0);
          }
        try
          {
            NodeObject arg0Copy = (NodeObject) Util.copyObject(arg0, _orb());
            NodeObject result = ((RMI_test) so.servant).exchangeNodeObject(arg0Copy);
            return (NodeObject) Util.copyObject(result, _orb());
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passArrayOfRemotes(RMI_test[] arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passArrayOfRemotes", true);
                out.write_value(cast_array(arg0), RMI_test[].class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passArrayOfRemotes(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passArrayOfRemotes",
          RMI_test.class);
        if (so == null)
          {
            return passArrayOfRemotes(arg0);
          }
        try
          {
            RMI_test[] arg0Copy = (RMI_test[]) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passArrayOfRemotes(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public RMI_test passReturnRemote(RMI_test arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("passReturnRemote", true);
                Util.writeRemoteObject(out, arg0);
                in = _invoke(out);
                return (RMI_test) PortableRemoteObject.narrow(in.read_Object(),
                  RMI_test.class);
              }
            catch (ApplicationException ex)
              {
                in = ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passReturnRemote(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passReturnRemote",
          RMI_test.class);
        if (so == null)
          {
            return passReturnRemote(arg0);
          }
        try
          {
            RMI_test arg0Copy = (RMI_test) Util.copyObject(arg0, _orb());
            RMI_test result = ((RMI_test) so.servant).passReturnRemote(arg0Copy);
            return (RMI_test) Util.copyObject(result, _orb());
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String getEgo()
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                OutputStream out = _request("_get_ego", true);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return getEgo();
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("_get_ego", RMI_test.class);
        if (so == null)
          {
            return getEgo();
          }
        try
          {
            return ((RMI_test) so.servant).getEgo();
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  public String passCollection(Collection arg0)
    throws RemoteException
  {
    if (!Util.isLocal(this))
      {
        try
          {
            org.omg.CORBA_2_3.portable.InputStream in = null;
            try
              {
                org.omg.CORBA_2_3.portable.OutputStream out = (org.omg.CORBA_2_3.portable.OutputStream) _request(
                  "passCollection", true);
                out.write_value((Serializable) arg0, Collection.class);
                in = (org.omg.CORBA_2_3.portable.InputStream) _invoke(out);
                return (String) in.read_value(String.class);
              }
            catch (ApplicationException ex)
              {
                in = (org.omg.CORBA_2_3.portable.InputStream) ex.getInputStream();
                String id = in.read_string();
                throw new UnexpectedException(id);
              }
            catch (RemarshalException ex)
              {
                return passCollection(arg0);
              }
            finally
              {
                _releaseReply(in);
              }
          }
        catch (SystemException ex)
          {
            throw Util.mapSystemException(ex);
          }
      }
    else
      {
        ServantObject so = _servant_preinvoke("passCollection", RMI_test.class);
        if (so == null)
          {
            return passCollection(arg0);
          }
        try
          {
            Collection arg0Copy = (Collection) Util.copyObject(arg0, _orb());
            return ((RMI_test) so.servant).passCollection(arg0Copy);
          }
        catch (Throwable ex)
          {
            Throwable exCopy = (Throwable) Util.copyObject(ex, _orb());
            throw Util.wrapException(exCopy);
          }
        finally
          {
            _servant_postinvoke(so);
          }
      }
  }

  // This method is required as a work-around for
  // a bug in the JDK 1.1.6 verifier.

  private Serializable cast_array(Object obj)
  {
    return (Serializable) obj;
  }
}
