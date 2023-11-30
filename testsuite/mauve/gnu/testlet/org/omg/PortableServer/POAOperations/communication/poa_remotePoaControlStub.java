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

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

/**
  * The remote POA control interface.
  */
public class poa_remotePoaControlStub
  extends org.omg.CORBA.portable.ObjectImpl
  implements remotePoaControl
{
  /**
   * Set the name of POA to that subsequent operations
   * will apply. This POA must be the child of the POA
   * to that this remotePoaControl servant is connected.
   */
  public void setControlTarget(String child_poa_name)
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("setControlTarget", true);
        out.write_string(child_poa_name);
        in = _invoke(out);
        return;
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException remarsh)
      {
        setControlTarget(child_poa_name);
      }
    finally
      {
        _releaseReply(in);
      }
  } // setControlTarget

  /**
   * Set the mode of the POA being controlled (active,
   * holding, discarding, deactivated).
   */
  public void setPoaMode(int mode)
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("setPoaMode", true);
        out.write_long(mode);
        in = _invoke(out);
        return;
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException remarsh)
      {
        setPoaMode(mode);
      }
    finally
      {
        _releaseReply(in);
      }
  } // setPoaMode

  /**
   * Get the mode of POA being controlled.
   */
  public int getPoaMode()
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("getPoaMode", true);
        in = _invoke(out);
        return in.read_long();
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();
        throw new MARSHAL(ex.getId());
      }
    catch (RemarshalException remarsh)
      {
        return getPoaMode();
      }
    finally
      {
        _releaseReply(in);
      }
  } // getPoaMode

  private static String[] ids = { remotePoaControlHelper.id() };

  public String[] _ids()
  {
    return ids;
  }
}