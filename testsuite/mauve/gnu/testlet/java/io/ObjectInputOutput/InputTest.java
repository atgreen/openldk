// Tags: JDK1.2
// Uses: Test

/* InputTest.java -- Tests ObjectInputStream class

   Copyright (c) 1999 by Free Software Foundation, Inc.
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
import java.io.IOException;
import java.io.ObjectInputStream;

public class InputTest implements Testlet
{
  public void test (TestHarness harness)
  {
    this.harness = harness;

    Test[] tests = Test.getValidTests ();
    for (int i = 0; i < tests.length; ++ i)
      test (tests[i]);
  }

  void test (Test t)
  {
    String cname = t.getClass ().getName ();
    harness.checkPoint (cname);
    ObjectInputStream ois = null;
    
    try
    {
      ois = new ObjectInputStream (
	harness.getResourceStream (cname.replace ('.', '#') + ".data"));

      Object[] objs = t.getTestObjs ();
      for (int i = 0; i < objs.length; ++ i)
	harness.check (ois.readObject (), objs[i]);
    }
    catch (Exception e)
    {
      harness.debug (e);
      harness.check (false);
      return;
    }
    finally
    {
      if (ois != null)
      {
	try
	{
	  ois.close ();
	}
	catch (IOException e) {}
      }
    }
  }
  
  TestHarness harness;
}
