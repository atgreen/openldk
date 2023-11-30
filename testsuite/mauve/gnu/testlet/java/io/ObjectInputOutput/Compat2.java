// Tags: JDK1.2
// Uses: SerBase

/* Compat2.java -- Test for Put/GetField.

   Copyright (c) 2003 by Free Software Foundation, Inc.
   Written by Guilhem Lavaux (guilhem@kaffe.org).
   Based on a test by Pat Tullmann <pat_kaffe@tullmann.org>.

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

package gnu.testlet.java.io.ObjectInputOutput;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Serializable;

public class Compat2 implements Testlet
{
  static String SERIAL_REFERENCE = "serial.bin";
  static String SERIAL_SCRATCH_FILENAME = "Compat2.tmp";
  static int SERIAL_REF_ID = 0;

  private static class GetTypeMismatch // object typemismatch in get
    implements Serializable
  {
    // Explicitly set serialVersionUID for different compilers handling
    // of inner classes.
    private static final long serialVersionUID = -2330048339523627109L;

    private Integer x = new Integer(17);
    private Integer y = new Integer(27);
    
    public String toString()
    {
      return (this.getClass().getName() + ": " +x+ "," +y);
    }
    
    private void writeObject(ObjectOutputStream stream) 
      throws IOException, ClassNotFoundException
    {
      ObjectOutputStream.PutField pf1 = stream.putFields();
      pf1.put("x", this.x);
      pf1.put("y", this.y);
      stream.writeFields();
    }
    
    private void readObject(ObjectInputStream stream) 
      throws IOException, ClassNotFoundException
    {
      ObjectInputStream.GetField gf1 = stream.readFields();
      this.x = (Integer)gf1.get("x", new String("Missed X?"));
      this.y = (Integer)gf1.get("y", new String("Missed Y?"));
    }
  }
  
  void generate(String fname) throws IOException
  { 
    FileOutputStream of = new FileOutputStream (fname);
    ObjectOutputStream oos = new ObjectOutputStream (of);
  
    oos.writeObject (new GetTypeMismatch());
  }

  GetTypeMismatch readSerial(String fname) throws IOException, ClassNotFoundException
  {
    FileInputStream ifs = new FileInputStream (fname);
    ObjectInputStream ios = new ObjectInputStream (ifs);
    
    return (GetTypeMismatch)ios.readObject();
  }

  public void test(TestHarness t)
  {
    int rand_id = 0;

    t.checkPoint ("Compatibility test for type mismatch when calling get methods");

    try
      {
	generate (SERIAL_SCRATCH_FILENAME);
	t.check (true);

	try
	  {
	    readSerial (SERIAL_SCRATCH_FILENAME);
	    t.check (false);
	    t.debug ("This should have triggered IllegalArgumentException");
	  }
	catch (Exception e)
	  {
	    if (e instanceof IllegalArgumentException)
	      t.check(true);
	    else
	      {
		t.check(false);
		t.debug("Expected IllegalArgumentException, not: " + e);
	      }
	  }
      }
    catch (Exception e)
      {
	t.check (false);
	t.debug (e);
      }
    
    try
      {
	ObjectInputStream ois = new ObjectInputStream (t.getResourceStream 
				  (getClass().getName().replace ('.', '#') + "." + SERIAL_REFERENCE));

	ois.readObject();
	t.check (false);
	t.debug ("This should have triggered IllegalArgumentException");
      }
    catch (Exception e)
      {
	if (e instanceof IllegalArgumentException)
	  t.check(true);
	else
	  {
	    t.check(false);
	    t.debug("Expected IllegalArgumentException, not: " + e);
	  }
      }
  }

  static public void main(String args[]) throws IOException
  {
    new Compat2().generate (SERIAL_REFERENCE);
  }
}
