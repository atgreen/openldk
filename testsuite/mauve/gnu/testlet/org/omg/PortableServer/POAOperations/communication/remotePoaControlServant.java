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

import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.CompletionStatus;
import org.omg.CORBA.OBJ_ADAPTER;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAManagerPackage.AdapterInactive;
import org.omg.PortableServer.POAManagerPackage.State;
import org.omg.PortableServer.POAPackage.AdapterNonExistent;

/**
 * Implements the remote POA control servant.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class remotePoaControlServant
  extends remotePoaControlPOA
{
  POA target;
  public byte[] target_object_id;

  /**
   * Finds the child target POA to control, by name.
   */
  public void setControlTarget(String child_poa_name)
  {
    try
      {
        target = _poa().find_POA(child_poa_name, false);
      }
    catch (AdapterNonExistent ex)
      {
        ex.printStackTrace();
        throw new BAD_PARAM();
      }
  }

  /**
   * Get the state of the target POA that must be previously set.
   */
  public int getPoaMode()
  {
    if (target == null)
      throw new BAD_INV_ORDER();
    return target.the_POAManager().get_state().value();
  }

  /**
   * Set the state of the target POA that must be previously set.
   *
   * @param mode the required POA mode plus:
   * 100 = deactivate default associated object.
   * 200 = activate default associated object.
   */
  public void setPoaMode(int mode)
  {
    if (target == null)
      throw new BAD_INV_ORDER();

    POAManager manager = target.the_POAManager();

    try
      {
        switch (mode)
          {
            case State._ACTIVE :
              manager.activate();
              break;

            case State._HOLDING :
              manager.hold_requests(false);
              break;

            case State._DISCARDING :
              manager.discard_requests(false);
              break;

            case State._INACTIVE :
              manager.deactivate(false, false);
              break;

            case 100 :
              try
                {
                  target.deactivate_object(target_object_id);
                }
              catch (Exception ex)
                {
                  System.err.println("Unable to deactivate");
                  ex.printStackTrace();
                }
              break;

            case 200 :
              try
                {
                  target.activate_object_with_id(target_object_id, null);
                }
              catch (Exception ex)
                {
                  throw new RuntimeException("Unable to activate", ex);
                }
              break;

            default :
              throw new BAD_PARAM();
          }
      }
    catch (AdapterInactive ex)
      {
        throw new OBJ_ADAPTER("Inactive", 0x5001, CompletionStatus.COMPLETED_YES);
      }
  }
}