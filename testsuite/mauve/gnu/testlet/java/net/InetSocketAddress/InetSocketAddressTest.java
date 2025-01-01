// Tags: JDK1.4

/*
   Copyright (C) 2004 Michael Koch

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.net.InetSocketAddress;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;

public class InetSocketAddressTest implements Testlet
{
  protected static TestHarness harness;
  
  public void test_Constructors()
  {
    harness.checkPoint("Constructors");

    InetSocketAddress sa = null;

    try
      {
	sa = new InetSocketAddress (InetAddress.getLocalHost(), 1234);

	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 0 "
		      + "Should not throw Exception here");
      }

    try
      {
	sa = new InetSocketAddress ((InetAddress) null, 80);

	harness.check (sa.getAddress().toString().equals ("0.0.0.0/0.0.0.0"),
		       "Error : test_Constructors failed - 1 "
		       + "No wildcard address returned");
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 1 "
		      + "Unexpected Exception here");
      }

    try
      {
	sa = new InetSocketAddress (InetAddress.getLocalHost(), -1);

	harness.fail ("Error: test_Contructors failed - 2 " +
		      "IllegalArgumentException expected here");
      }
    catch (IllegalArgumentException e)
      {
	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 2 "
		      + "Unexpected Exception here");
      }

    try

      {
	sa = new InetSocketAddress (InetAddress.getLocalHost(), 65536);

	harness.fail ("Error: test_Contructors failed - 3 "
		      + "IllegalArgumentException expected here");
      }
    catch (IllegalArgumentException e)
      {
	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 3 "
		      + "Unexpected Exception here");
      }

    try
      {
	sa = new InetSocketAddress (-1);

	harness.fail ("Error: test_Contructors failed - 4 "
		      + "IllegalArgumentException expected here");
      }
    catch (IllegalArgumentException e)
      {
	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 4 "
		      + "Unexpected Exception here");
      }

    try
      {
	sa = new InetSocketAddress (65536);

	harness.fail ("Error: test_Contructors failed - 5 "
		      + "IllegalArgumentException expected here");
      }
    catch (IllegalArgumentException e)
      {
	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 5 "
		      + "Unexpected Exception here");
      }

    try
      {
	sa = new InetSocketAddress ((String) null, 80);

	harness.fail ("Error: test_Contructors failed - 7 "
		      + "IllegalArgumentException expected here");
      }
    catch (IllegalArgumentException e)
      {
	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 7 "
		      + "Unexpected Exception here");
      }

    try
      {
	sa = new InetSocketAddress ("localhost", -1);

	harness.fail ("Error: test_Contructors failed - 8 "
		      + "IllegalArgumentException expected here");
      }
    catch (IllegalArgumentException e)
      {
	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 8 "
		      + "Unexpected Exception here");
      }

    try
      {
	sa = new InetSocketAddress ("localhost", 65536);

	harness.fail ("Error: test_Contructors failed - 9 "
		      + "IllegalArgumentException expected here");
      }
    catch (IllegalArgumentException e)
      {
	harness.check (true);
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Constructors failed - 9 "
		      + "Unexpected Exception here");
      }
  }

  public void test_Basics()
  {
    harness.checkPoint("Basics");

    InetSocketAddress sa = null;

    sa = new InetSocketAddress ("localhost", 80);

    harness.check (sa.getPort() == 80, "Error : test_Basics failed - 1"
		   + " Returned wrong port number");

    harness.check (sa.getHostName().equals("localhost"), "Error : test_Basics failed - 2"
		   + " Returned wrong host name");

    try
      {
	byte[] ipaddr = { (byte) 127, (byte) 0, (byte) 0, (byte) 1 };
	harness.check (sa.getAddress().equals(InetAddress.getByAddress ("localhost", ipaddr)), "Error : test_Basics failed - 3"
		       + " Returned wrong InetAdress object");
      }
    catch (UnknownHostException e)
      {
	harness.fail ("Error : test_Basics failed - 3"
		      + " Unexpected UnknownHostException");
      }

    try
      {
	byte[] ipaddr = { (byte) 1, (byte) 2, (byte) 3, (byte) 4 };
	sa = new InetSocketAddress (InetAddress.getByAddress("foo.bar", ipaddr), 80);

	harness.check (! sa.isUnresolved(), "Error : test_Basics failed - 4"
		       + " Unresolveable hostname got resolved");
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Basics failed - 4"
		      + " Unexpected UnknownHostException");
      }

    try
      {
	sa = new InetSocketAddress ("gcc.gnu.org", 80);
	harness.check (! sa.isUnresolved(), "Error : test_Basics failed - 5"
		       + " Resolveable hostname got not resolved");
      }
    catch (Exception e)
      {
	harness.fail ("Error : test_Basics failed - 5"
		      + " Unexpected UnknownHostException");
      }
  }
    
  public void testall()
  {
    test_Constructors();
    test_Basics();
  }

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
