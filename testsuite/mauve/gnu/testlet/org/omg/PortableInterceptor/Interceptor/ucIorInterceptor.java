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

import org.omg.CORBA.LocalObject;
import org.omg.CORBA.Policy;
import org.omg.IOP.TaggedComponent;
import org.omg.PortableInterceptor.IORInfo;
import org.omg.PortableInterceptor.IORInterceptor;
import org.omg.PortableServer.SERVANT_RETENTION_POLICY_ID;
import org.omg.PortableServer.ServantRetentionPolicy;
import org.omg.PortableServer.ServantRetentionPolicyValue;

/**
 * Our IOR interceptor.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class ucIorInterceptor extends LocalObject implements IORInterceptor
{
  public static boolean destroyed;
  public static boolean policyOK;

  /**
   * Get class name as name of this test interceptor.
   */
  public String name()
  {
    return getClass().getName();
  }

  /**
   * Print message.
   */
  public void destroy()
  {
    destroyed = true;
  }

  public void establish_components(IORInfo info)
  {
    TaggedComponent component = new TaggedComponent();

    component.tag = 0x452572;

    byte[] data = new byte[ 0x20 ];
    for (byte i = 0; i < data.length; i++)
      {
        data [ i ] = i;
      }
    component.component_data = data;

    info.add_ior_component(component);
    info.add_ior_component_to_profile(component, 0);

    Policy p = info.get_effective_policy(SERVANT_RETENTION_POLICY_ID.value);

    policyOK =
      ((ServantRetentionPolicy) p).value() == ServantRetentionPolicyValue.RETAIN;
  }
}