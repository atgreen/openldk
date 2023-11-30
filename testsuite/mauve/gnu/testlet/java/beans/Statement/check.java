// Test Statement.check().

// Written by Jerry Quinn <jlquinn@optonline.net>

// This file is part of Mauve.

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
// Boston, MA 02111-1307, USA.

// Tags: JDK1.4

// Test features added by JDK 1.4

package gnu.testlet.java.beans.Statement;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.beans.Statement;

public class check implements Testlet
{
  public static int x = 0;

  public static void test1(int y) { x = y; }

  public class dummy
  {
    public String s;
    public int x;
    public dummy() { s = ""; x = 5;}

    public String method(String s)
    {
      this.s = s;
      return this.s + " " + this.x;
    }

    public String method(String s, int y)
    {
      this.s = s;
      this.x = y + 1;
      return this.s + " " + this.x;
    }

    public String method1(String s, Integer y)
    {
      this.s = s;
      this.x = y.intValue();
      return this.s + " " + this.x;
    }

//     public static String method2(String s)
//     {
//       return s + "static";
//     }
  }

  public void test (TestHarness harness)
  {
    dummy d = new dummy();

    // Check that we can create and execute a statement
    Object arg1[] = {"test"};
    Statement stmt = new Statement(d, "method", arg1);
    harness.check(stmt != null);
    harness.check(stmt.getTarget() == d);
    harness.check(stmt.getMethodName().equals("method"));

    try
      {
	stmt.execute();
      }
    catch (Exception e)
      {
	harness.fail("Statement execute");
      }

    harness.check(d.s.equals("test"));
    harness.check(d.x == 5);

    // Check that we can create and execute a statement and that a
    // wrapper class resolves to a method taking a primitive.
    Object arg2[] = {"test", new Integer(6) };
    stmt = new Statement(d, "method", arg2);
    harness.check(stmt != null);

    Object stmtArgs[] = stmt.getArguments();
    harness.check(stmtArgs.length == arg2.length && stmtArgs[0] == arg2[0]
		  && stmtArgs[1] == arg2[1]);

    try
      {
	stmt.execute();
      }
    catch (Exception e)
      {
	harness.fail("Statement execute");
      }

    harness.check(d.s.equals("test"));
    harness.check(d.x == 7);


    // Check that we can create and execute a statement for a static method.
    Object arg3[] = {new Integer(1)};
    stmt = new Statement(this, "test1", arg3);
    
    try
      {
	stmt.execute();
      }
    catch (Exception e)
      {
	harness.fail("Static method execute " + e.toString());
      }
    harness.check(x == 1);


    // Check that we can call get and set on an array object in a statement
    int iarray[] = {1, 2, 3, 4, 5};
    Object arg4[] = { new Integer(2), new Integer(6) };

    stmt = new Statement(iarray, "set", arg4);
    try
      {
	stmt.execute();
      }
    catch (Exception e)
      {
	harness.fail("Array set");
      }

    Object arg5[] = { new Integer(2) };
    stmt = new Statement(iarray, "get", arg5);
    
    try
      {
	stmt.execute();
      }
    catch (Exception e)
      {
	harness.fail("Array get");
      }

    harness.check(iarray[2] == 6);
  }

}
