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
import org.omg.PortableInterceptor.ORBInitInfo;
import org.omg.PortableInterceptor.ORBInitInfoPackage.DuplicateName;
import org.omg.PortableInterceptor.ORBInitializer;

/**
 * Registers our interceptors.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class ucInitialiser extends LocalObject implements ORBInitializer
{
  public static boolean preInit;
  public static boolean postInit;
  public static int slot_0;
  public static int slot_1;

  public void pre_init(ORBInitInfo info)
  {
    try
      {
        preInit = true;
        info.add_ior_interceptor(new ucIorInterceptor());
        info.add_server_request_interceptor(new ucServerRequestInterceptor());
        info.add_client_request_interceptor(new ucClientRequestInterceptor());

        slot_0 = info.allocate_slot_id();
        slot_1 = info.allocate_slot_id();
      }
    catch (DuplicateName ex)
      {
        ex.printStackTrace();
      }
  }

  public void post_init(ORBInitInfo info)
  {
    postInit = true;
  }
}