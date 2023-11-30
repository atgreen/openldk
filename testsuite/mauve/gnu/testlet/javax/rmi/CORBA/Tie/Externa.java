// Not a test, required by RMI_IIOP.java.

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

package gnu.testlet.javax.rmi.CORBA.Tie;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

/**
 * This file is part of the CORBA RMI over IIOP the test executable
 * class being gnu.testlet.javax.rmi.CORBA.Tie.RMI_IIOP. It is an Externalizable
 * being passed via RMI-IIOP.
 *
 * @author Audrius Meskauskas (AudriusA@bluewin.ch)
 */
public class Externa
  implements Externalizable
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;

  public int a;

  public int b;

  String sx;

  public String toString()
  {
    return sx;
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    a = in.readInt();
    b = in.readInt();
    sx = "(ex " + a + ":" + b + ")";
  }

  public void writeExternal(ObjectOutput out)
    throws IOException
  {
    out.writeInt(a);
    out.writeInt(b);
  }

}
