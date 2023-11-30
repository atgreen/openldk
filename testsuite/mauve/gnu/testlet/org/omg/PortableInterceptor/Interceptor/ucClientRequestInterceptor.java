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
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.LocalObject;
import org.omg.CORBA.ORB;
import org.omg.IOP.ServiceContext;
import org.omg.PortableInterceptor.ClientRequestInfo;
import org.omg.PortableInterceptor.ForwardRequest;

/**
 * A sample client request interceptor.
 */
public class ucClientRequestInterceptor extends LocalObject
  implements org.omg.PortableInterceptor.ClientRequestInterceptor
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

  public void receive_exception(ClientRequestInfo info)
    throws ForwardRequest
  {
    callPattern.append("Ex");

    // _get_theField is handled in the forwarded target without interceptors.
    if (!info.operation().equals("_get_theField"))
      {
        ServiceContext s = info.get_reply_service_context(6000);
        String ps = new String(s.context_data);
        if (s.context_id != 6000)
          {
            throw new RuntimeException(
              "C ERROR Returned context 6000 id mismatch"
            );
          }

        String p = callPattern.toString();

        if (!(
            "SRq[throwException]Ex".equals(p) ||
            "SRq[passCharacters]Ex".equals(p)
          )
        )
          {
            throw new RuntimeException("Invalid client pattern " + p);
          }

        if (!(
            "InRq[throwException]Ex".equals(ps) ||
            "InRq[passCharacters]Ex".equals(ps)
          )
        )
          {
            throw new RuntimeException("Invalid server pattern " + ps);
          }
      }
  }

  public void receive_other(ClientRequestInfo info) throws ForwardRequest
  {
    callPattern.append("Fx");
  }

  public void receive_reply(ClientRequestInfo info)
  {
    callPattern.append("Rr");
    checkServerCtx(info);

    // Check if the two server side service contexts are present.
    // _get_theField is handled in the forwarded target without interceptors.
    if (!info.operation().equals("_get_theField"))
      {
        ServiceContext s = info.get_reply_service_context(6000);
        String ps = new String(s.context_data);
        if (s.context_id != 6000)
          {
            throw new RuntimeException(
              "C ERROR Returned context 6000 id mismatch"
            );
          }

        if (!"SRq[sayHello]Rr".equals(callPattern.toString()))
          {
            throw new RuntimeException("Invalid client pattern " +
              callPattern
            );
          }

        if (!"InRq[sayHello]Rp".equals(ps))
          {
            throw new RuntimeException("Invalid server pattern " + ps);
          }
      }
  }

  private void checkServerCtx(ClientRequestInfo info)
  {
    // _get_theField is handled in the forwarded target without interceptors.
    if (!info.operation().equals("_get_theField"))
      {
        ServiceContext se = info.get_reply_service_context(6001);
        if (se.context_data.length != 0)
          {
            throw new RuntimeException(
              "C ERROR Server side context 6001 is not present in reply."
            );
          }

        ServiceContext sx = info.get_reply_service_context(6002);
        if (sx.context_data.length != 0)
          {
            throw new RuntimeException(
              "C ERROR Server side context 6001 is not present in reply."
            );
          }

        if (se.context_id != 6001)
          {
            throw new RuntimeException(
              "C ERROR Returned context 6001 id mismatch"
            );
          }
      }
    else
      {
        // Handled by the forwarded target without interceptors. No context
        // should
        // be added.
        try
          {
            info.get_reply_service_context(6000);
            throw new RuntimeException(
              "C ERROR context 6000 present where it should not be"
            );
          }
        catch (BAD_PARAM e)
          {
            // Excepected.
          }
      }
  }

  public void send_poll(ClientRequestInfo info)
  {
    callPattern.append("Sp");
  }

  /**
   * Add a sample service context.
   */
  public void send_request(ClientRequestInfo info) throws ForwardRequest
  {
    try
      {
        Any is0 = info.get_slot(ucInitialiser.slot_0);

        if (!is0.extract_string().equals("Initial value for slot 0"))
          {
            throw new RuntimeException("Wrong initial slot 0 value");
          }

        Any is1 = info.get_slot(ucInitialiser.slot_1);
        if (!is1.extract_string().equals("Initial value for slot 1"))
          {
            throw new RuntimeException("Wrong initial slot 1 value");
          }

        Any s0 = ORB.init().create_any();
        Any s1 = ORB.init().create_any();

        s0.insert_string("Client slot zero");
        s1.insert_string("Client slot one");
      }
    catch (Exception e)
      {
        throw new RuntimeException("Client Slot problem", e);
      }

    callPattern.setLength(0);
    callPattern.append("SRq[" + info.operation() + "]");

    // One with content.
    ServiceContext c = new ServiceContext();
    c.context_id = 5000;
    c.context_data = "my_request_context_1".getBytes();

    info.add_request_service_context(c, false);

    // Another empty.
    ServiceContext ce = new ServiceContext();
    ce.context_id = 5001;
    ce.context_data = new byte[ 0 ];

    info.add_request_service_context(ce, false);
  }
}