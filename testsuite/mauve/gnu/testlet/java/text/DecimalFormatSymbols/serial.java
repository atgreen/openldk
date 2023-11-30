// serial.java -- Checks that object can bee serialized and deserialized.
//
// Copyright (c) 2003 Mark J. Wielaard (mark@klomp.org)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published 
// by the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA

// Tags: JDK1.2

package gnu.testlet.java.text.DecimalFormatSymbols;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

public class serial implements Testlet
{
  private static String infinity = "supermuch";
  private static String nan = "Ehe?";

  public void test(TestHarness harness)
  {
    DecimalFormatSymbols dfs1 = new DecimalFormatSymbols(Locale.US);
    dfs1.setInfinity(infinity);
    dfs1.setNaN(nan);

    // Serialize and Deserialize.e
    Object o = null;
    try
      {
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	ObjectOutputStream oos = new ObjectOutputStream(baos);
	
	oos.writeObject(dfs1);
	oos.close();
	
	byte[] bs = baos.toByteArray();
	ByteArrayInputStream bois = new ByteArrayInputStream(bs);
	ObjectInputStream ois = new ObjectInputStream(bois);
	o = ois.readObject();
	ois.close();
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
      }
    catch (ClassNotFoundException cnfe)
      {
	harness.debug(cnfe);
      }

    DecimalFormatSymbols dfs2 = (DecimalFormatSymbols) o;
    harness.check(dfs1, dfs2);

    harness.check(dfs2.getInfinity(), infinity);
    harness.check(dfs2.getNaN(), nan);
  }
  
}

