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

package gnu.testlet.org.omg.CORBA.portable.OutputStream;

import org.omg.CORBA.OctetSeqHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.InvokeHandler;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.ResponseHandler;

import java.io.ByteArrayOutputStream;

/**
 * A reflector is a non standard CORBA object that accepts the
 * passed parameter and then returns all passed data as a
 * plain byte array, exactly how they were sent. Not a test,
 * needed by BinaryAlignment.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
class mirror
  extends ObjectImpl
  implements InvokeHandler
{
  /**
   *  Return the passed parameters as the plain binary array.
   */
  public OutputStream _invoke(String method, InputStream in, ResponseHandler rh)
  {
    ByteArrayOutputStream data = new ByteArrayOutputStream();

    byte b;
    do
      {
        b = in.read_octet();
        data.write(b);
      }
    while ((b & 0xFF) != 0xFF);

    OutputStream out = rh.createReply();
    OctetSeqHelper.write(out, data.toByteArray());

    return out;
  }

  public String[] _ids()
  {
    return new String[] { getClass().getName() };
  }
}
