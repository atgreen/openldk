// Tags: JDK1.2

/* VectorSerialization.java -- Tests the serialization of Vector.

   Copyright (c) 1999 by Free Software Foundation, Inc.
   Written by Guilhem Lavaux <guilhem@kaffe.org>.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation, version 2. (see COPYING)

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation
   Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA */

package gnu.testlet.java.util.Vector;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.util.Vector;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

public class VectorSerialization implements Testlet
{
	public static void main(String args[]) throws IOException
	{
	  FileOutputStream os = new FileOutputStream("TestVector.ser");
	  ObjectOutputStream oo = new ObjectOutputStream(os);
	  Vector v;
	 
	  v = new Vector();
	  v.add(new Integer(10));
	  oo.writeObject(v);

	  v = new Vector();
	  v.add(new Integer(20));
	  oo.writeObject(v);
	  
	  os.close();
	}
	
	public void test(TestHarness harness)
	{
	  try
	    {
	      String packageName = getClass().getPackage().getName().replace('.','#');
	      InputStream is = harness.getResourceStream(packageName + "#TestVector.ser");
	      ObjectInputStream oi = new ObjectInputStream(is);
	      Vector v;
	 
	      v = (Vector) oi.readObject();
	      harness.check(v != null, "returned object null ?");
	      harness.check(v.size(), 1);
	      harness.check(v.get(0), new Integer(10));
	      v = (Vector) oi.readObject();
	      harness.check(v != null, "returned object null ?");
	      harness.check(v.size(), 1);
	      harness.check(v.get(0), new Integer(20));
	    }
	  catch (Exception e)
	    {
	      harness.fail("Caught an unexpected exception");
	      harness.debug(e);
	    }
	}
}
