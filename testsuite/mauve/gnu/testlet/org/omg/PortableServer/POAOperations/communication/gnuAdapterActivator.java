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

import org.omg.CORBA.LocalObject;
import org.omg.CORBA.Policy;
import org.omg.PortableServer.AdapterActivator;
import org.omg.PortableServer.POA;

/**
 * Defines a simple adapter activator.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuAdapterActivator
  extends LocalObject
  implements AdapterActivator
{
  /**
   * Create a new POA on the parent, using the parent policy set
   * from the suitable parent of grandparend and with independent
   * POA manager (passing null to the createPOA).
   *
   * @param parent a parent. Either this parent or one of its
   * grandparents must be gnuAbstractPOA, able to provide a
   * policy set.
   *
   * @param child_name the name of the child being created.
   *
   * @return true on success or false if no gnuAbstractPOA
   * found till the root poa.
   */
  public boolean unknown_adapter(POA parent, String child_name)
  {
    try
      {
        POA n = parent.create_POA(child_name, null, new Policy[ 0 ]);
        n.the_POAManager().activate();
      }
    catch (Exception ex)
      {
        return false;
      }
    return true;
  }
}