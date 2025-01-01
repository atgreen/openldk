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

package gnu.testlet.org.omg.CORBA.ORB.Asynchron;

import org.omg.CORBA.ORB;

/**
 * The server for handling parallel submissions.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class assServer
{
  public static String[] start_server(int n)
  {
    try
      {
        // Create and initialize the ORB.
        final ORB orb = ORB.init(new String[0], null);

        String[] iors = new String[ n ];

        for (int i = 0; i < iors.length; i++)
          {
            // Create the servant and register it with the ORB.
            assServant tester = new assServant();
            orb.connect(tester);
            iors [ i ] = orb.object_to_string(tester);
          }

        new Thread()
          {
            public void run()
            {
              // Start the thread, serving the invocations from clients.
              orb.run();
            }
          }.start();

        return iors;
      }
    catch (Exception e)
      {
        System.err.println("ERROR: " + e);
        e.printStackTrace(System.out);
        return null;
      }
  }
}