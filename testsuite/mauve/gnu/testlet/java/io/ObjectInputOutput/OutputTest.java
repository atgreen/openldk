// Tags: JDK1.2
// Uses: Test

/* OutputTest.java -- Tests ObjectOutputStream class

   Copyright (c) 1999, 2003 by Free Software Foundation, Inc.
   Written by Geoff Berry <gcb@gnu.org>.

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
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;

public class OutputTest implements Testlet
{
  public void test (TestHarness harness)
  {
    this.harness = harness;

    Test[] tests = Test.getValidTests ();
    for (int i = 0; i < tests.length; ++ i)
      test (tests[i], false);

    tests = Test.getErrorTests ();
    for (int i = 0; i < tests.length; ++ i)
      test (tests[i], true);
  }

  void test (Test t, boolean throwsOSE)
  {
    String cname = t.getClass ().getName ();
    harness.checkPoint (cname);    

    try
    {
      ByteArrayOutputStream bytes = new ByteArrayOutputStream ();
      ObjectOutputStream oos = new ObjectOutputStream (bytes);
      Object[] objs = t.getTestObjs ();
      boolean exception_thrown = false;
      try
	{
	  for (int i = 0; i < objs.length; ++ i)
	    oos.writeObject(objs[i]);
	}
      catch (ObjectStreamException ose)
	{
	  exception_thrown = true;
	  if (!throwsOSE)
	    harness.debug(ose);
	}
      oos.close ();
      
      if (throwsOSE)
	harness.check(exception_thrown, "Unserializable: " + t);
      else
	{
	  harness.check(!exception_thrown, "Serializable: " + t);
	  harness.check (compareBytes (bytes.toByteArray (),
				   harness.getResourceStream (
				     cname.replace ('.', '#') + ".data")));
	}
    }
    catch (Exception e)
    {
      harness.debug (e);
      harness.check (false);
    }
  }

  boolean compareBytes (byte[] written_bytes, InputStream ref_stream)
    throws IOException
  {
    for (int data, i = 0; i < written_bytes.length; ++ i)
    {
      data = ref_stream.read ();   
      if (data == -1)
      {
	harness.debug ("Reference data is shorter than written data.");
	return false;
      }
      if ((byte)data != written_bytes[i])
      {
	harness.debug ("Data differs at byte " + i);
	harness.debug ("Ref. byte = 0"
	               + Integer.toOctalString (written_bytes[i] & 0xff)
		       + ", written byte = 0"
	               + Integer.toOctalString (data & 0xff));
	return false;
      }
    }

    if (ref_stream.read () != -1)
    {
      harness.debug ("Reference data is longer than written data.");
      return false;
    }
    else
      return true;
  }
  
  TestHarness harness;
}
