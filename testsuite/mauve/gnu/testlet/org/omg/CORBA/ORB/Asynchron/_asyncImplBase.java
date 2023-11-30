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

package gnu.testlet.org.omg.CORBA.ORB.Asynchron;

import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;

/**
 * The parallel submission handler implementation base (server side).
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
abstract class _asyncImplBase
  extends ObjectImpl
  implements async, InvokeHandler
{
  public OutputStream _invoke(String method,
                              org.omg.CORBA.portable.InputStream in,
                              org.omg.CORBA.portable.ResponseHandler rh
                             )
  {
    // Simplified case (only one method - no need for switch).
    org.omg.CORBA.portable.OutputStream out = null;
    int duration = in.read_long();
    int result = sleep_and_return(duration);
    out = rh.createReply();
    out.write_long(result);
    return out;
  }

  private static String[] __ids =
    { "IDL:test/org/omg/CORBA/Asynchron/async:1.0" };

  public String[] _ids()
  {
    return __ids;
  }
}
