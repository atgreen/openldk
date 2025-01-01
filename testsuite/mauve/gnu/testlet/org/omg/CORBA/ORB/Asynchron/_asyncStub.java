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

import org.omg.CORBA.MARSHAL;
import org.omg.CORBA.portable.ApplicationException;
import org.omg.CORBA.portable.Delegate;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.RemarshalException;

/**
 * The parallel submission handler implementation stub (client side).
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
class _asyncStub
  extends ObjectImpl
  implements async
{
  public _asyncStub(Delegate delegate)
  {
    super();
    _set_delegate(delegate);
  }

  /**
   *  Sleep for the given duration, when return.
   */
  public int sleep_and_return(int duration)
  {
    InputStream in = null;
    try
      {
        OutputStream out = _request("sleep_and_return", true);
        out.write_long(duration);
        in = _invoke(out);
        return in.read_long();
      }
    catch (ApplicationException ex)
      {
        in = ex.getInputStream();

        String _id = ex.getId();
        throw new MARSHAL(_id);
      }
    catch (RemarshalException _rm)
      {
        return sleep_and_return(duration);
      }
    finally
      {
        _releaseReply(in);
      }
  }

  private static String[] __ids =
    { "IDL:test/org/omg/CORBA/Asynchron/async:1.0" };

  public String[] _ids()
  {
    return (String[]) __ids.clone();
  }
}
