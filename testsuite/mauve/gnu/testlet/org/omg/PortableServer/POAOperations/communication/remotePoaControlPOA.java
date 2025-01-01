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

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;
import org.omg.PortableServer.Servant;

import java.util.*;

/**
  * The remote POA control interface.
  */
public abstract class remotePoaControlPOA
  extends org.omg.PortableServer.Servant
  implements remotePoaControlOperations, InvokeHandler
{
  private static Hashtable _methods = new Hashtable();

  static
  {
    _methods.put("setControlTarget", new Integer(0));
    _methods.put("setPoaMode", new Integer(1));
    _methods.put("getPoaMode", new Integer(2));
  }

  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    OutputStream out = null;
    Integer __method = (Integer) _methods.get(method);
    if (__method == null)
      throw new BAD_OPERATION(method, 0, CompletionStatus.COMPLETED_MAYBE);

    switch (__method.intValue())
      {
        /**
         * Set the name of POA to that subsequent operations
         * will apply. This POA must be the child of the POA
         * to that this remotePoaControl servant is connected.
         */
        case 0 : // gnu/classpath/examples/CORBA/SimpleCommunication/communication/remotePoaControl/setControlTarget
        {
          String child_poa_name = in.read_string();
          this.setControlTarget(child_poa_name);
          out = rh.createReply();
          break;
        }

        /**
             * Set the mode of the POA being controlled (active,
             * holding, discarding, deactivated).
             */
        case 1 : // gnu/classpath/examples/CORBA/SimpleCommunication/communication/remotePoaControl/setPoaMode
        {
          int mode = in.read_long();
          this.setPoaMode(mode);
          out = rh.createReply();
          break;
        }

        /**
             * Get the mode of POA being controlled.
             */
        case 2 : // gnu/classpath/examples/CORBA/SimpleCommunication/communication/remotePoaControl/getPoaMode
        {
          int result = getPoaMode();
          out = rh.createReply();
          out.write_long(result);
          break;
        }

        default :
          throw new BAD_OPERATION(0, CompletionStatus.COMPLETED_MAYBE);
      }

    return out;
  } // _invoke

  private static String[] ids = { remotePoaControlHelper.id() };

  public String[] _all_interfaces(org.omg.PortableServer.POA poa,
                                  byte[] objectId
                                 )
  {
    return ids;
  }

  public remotePoaControl _this()
  {
    return remotePoaControlHelper.narrow(super._this_object());
  }

  public remotePoaControl _this(ORB orb)
  {
    return remotePoaControlHelper.narrow(super._this_object(orb));
  }
} // class remotePoaControlPOA
