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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.


package gnu.testlet.org.omg.PortableInterceptor.Interceptor;

import org.omg.CORBA.Any;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.DATA_CONVERSION;
import org.omg.CORBA.INV_FLAG;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.IOP.ServiceContext;
import org.omg.PortableInterceptor.ForwardRequest;
import org.omg.PortableInterceptor.ServerRequestInfo;
import org.omg.PortableInterceptor.ServerRequestInterceptor;

/**
 * Our server request interceptor.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class ucServerRequestInterceptor extends LocalObject
  implements ServerRequestInterceptor
{
  StringBuffer callPattern = new StringBuffer();
  public static boolean destroyed;

  /**
   * Get class name as name of this test interceptor.
   */
  public String name()
  {
    return getClass().getName();
  }

  public void destroy()
  {
    destroyed = true;
  }

  public void receive_request_service_contexts(ServerRequestInfo info)
    throws ForwardRequest
  {
    try
      {
        Any s0 = ORB.init().create_any();
        Any s1 = ORB.init().create_any();

        s0.insert_string("Slot zero");
        s1.insert_string("Slot one");

        info.set_slot(ucInitialiser.slot_0, s0);
        info.set_slot(ucInitialiser.slot_1, s1);
      }
    catch (Exception e)
      {
        throw new RuntimeException("Server Slot problem", e);
      }

    callPattern.setLength(0);
    callPattern.append("In");

    checkClientCtx(info);

    // Add one context to reply.
    ServiceContext ce = new ServiceContext();
    ce.context_id = 6002;
    ce.context_data = new byte[ 0 ];

    info.add_reply_service_context(ce, true);
  }

  public void receive_request(ServerRequestInfo info) throws ForwardRequest
  {
    callPattern.append("Rq[" + info.operation() + "]");

    // Add one context to reply.
    ServiceContext ce = new ServiceContext();
    ce.context_id = 6001;
    ce.context_data = new byte[ 0 ];

    info.add_reply_service_context(ce, false);

    checkClientCtx(info);

    // Throw an exception on "passCharacters"
    if (info.operation().equals("passCharacters"))
      {
        throw new DATA_CONVERSION(87652, CompletionStatus.COMPLETED_MAYBE);
      }

    // Forward on the attempt to get the field.
    // The forwarding target is connected to another ORB without this
    // interceptor.
    if (info.operation().equals("_get_theField"))
      {
        throw new ForwardRequest(testInterceptors.fior);
      }
  }

  private void checkClientCtx(ServerRequestInfo info)
  {
    // Check if the two client side service contexts are present.
    ServiceContext s = info.get_request_service_context(5000);

    ServiceContext se = info.get_request_service_context(5001);

    if (s.context_id != 5000)
      {
        throw new RuntimeException("S ERROR Returned context 5000 id mismatch");
      }

    if (se.context_id != 5001)
      {
        throw new RuntimeException("S ERROR Returned context 5001 id mismatch");
      }
  }

  public void send_exception(ServerRequestInfo info) throws ForwardRequest
  {
    callPattern.append("Ex");
    addCallPattern(info);
    try
      {
        // Throw another exception, expecting replacement.
        if (info.sending_exception().type().id().equals("IDL:omg.org/CORBA/DATA_CONVERSION:1.0")
        )
          {
            throw new INV_FLAG(52, CompletionStatus.COMPLETED_MAYBE);
          }
      }
    catch (BadKind e)
      {
        e.printStackTrace();
      }
  }

  public void send_other(ServerRequestInfo info)
  {
    callPattern.append("Fw");
    addCallPattern(info);
  }

  public void send_reply(ServerRequestInfo info)
  {
    callPattern.append("Rp");
    addCallPattern(info);

    try
      {
        Any s0 = info.get_slot(ucInitialiser.slot_0);
        if (!s0.extract_string().equals("Slot zero"))
          {
            throw new RuntimeException("Slot 0 value mismatch");
          }

        Any s1 = info.get_slot(ucInitialiser.slot_1);
        if (!s1.extract_string().equals("Slot one"))
          {
            throw new RuntimeException("Slot 1 value mismatch");
          }
      }
    catch (Exception e)
      {
        throw new RuntimeException("Server slot problem", e);
      }
  }

  void addCallPattern(ServerRequestInfo info)
  {
    // Add one context.
    ServiceContext c = new ServiceContext();
    c.context_id = 6000;
    c.context_data = callPattern.toString().getBytes();

    info.add_reply_service_context(c, false);
  }
}