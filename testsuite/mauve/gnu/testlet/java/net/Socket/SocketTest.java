// Tags: JDK1.0
// Uses: SocketBServer SocketServer

/*
  Copyright (C) 1999 Hewlett-Packard Company
  Copyright (C) 2005 Mark J. Wielaard <mark@klomp.org>
  
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

package gnu.testlet.java.net.Socket;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;
import java.io.*;


public class SocketTest implements Testlet
{
  protected static TestHarness harness;
  
  public void test_BasicServer()
  {
    harness.checkPoint("BasicServer");
    try {
      SocketServer srv = new SocketServer();
      SocketServer.harness = harness;
      srv.init();
      srv.start();
      Thread.yield();
      harness.check(true, "BasicServer");
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 0 " +
		   "exception was thrown.");
      harness.debug(e);
    }
    
    Socket sock = null;
    try {
      sock = new Socket("127.0.0.1", 23000);
      DataInputStream dis = new DataInputStream(sock.getInputStream());
      String str = dis.readLine();
      
      harness.check(str.equals("hello buddy"),
		    "Error : test_BasicServer failed - 1 " +
		    "string returned is not correct.");
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 2 " +
		   "exception was thrown.");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch(IOException ignored) {}
    }
    
    // second iteration
    try {
      sock = new Socket("127.0.0.1", 23000);
      DataInputStream dis = new DataInputStream(sock.getInputStream());
      String str = dis.readLine();
      
      harness.check(str.equals("hello buddy"),
		    "Error : test_BasicServer failed - 3 " +
		    "string returned is not correct.");
      sock.close();
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 4 " +
		   "exception was thrown.");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch(IOException ignored) {}
    }
    
    // second iteration
    try {
      sock = new Socket("127.0.0.1", 23000);
      DataInputStream dis = new DataInputStream(sock.getInputStream());
   
      byte data[] = new byte[5];
      int len;

      len = dis.read(data);
      String str = new String(data, 0, 0, 5);

      harness.check(str.equals("hello"),
		    "Error : test_BasicServer failed - 5 " +
		    "string returned is not correct.");
      dis.close();
      sock.close();
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 6 " +
		   "exception was thrown.");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch(IOException ignored) {}
    }

    // second iteration
    try {
      sock = new Socket("127.0.0.1", 23000);
      InputStream is = sock.getInputStream();
      byte data[] = new byte[5];

      int len;
      len = is.read(data, 0, 5);
      String str= new String(data, 0, 0, 5);

      harness.check(str.equals("hello"),
		    "Error : test_BasicServer failed - 8 " +
		    "string returned is not correct.");
      is.close();
      harness.check(true);
      
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 9 " +
		   "exception was thrown.");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch(IOException ignored) {}
    }

    // second iteration
    try {
      sock = new Socket("127.0.0.1", 23000);
      InputStream is = sock.getInputStream();
      byte data[] = new byte[5];
      is.skip(2);

      int len = is.available();  // deterministic after blocking for skip
      harness.check(len > 0,
		    "Error : test_BasicServer failed - 7 " +
		     "no more data available");

      is.read(data, 0, 3);

      String str = new String(data, 0, 0, 3);

      harness.check(str.equals("llo"),
		    "Error : test_BasicServer failed - 10 " +
		    "string returned is not correct.");
      is.close();
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 11 " +
		   "exception was thrown.");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch(IOException ignored) {}
    }

  }

  public void test_params()
  {
    String host = harness.getMailHost();
    int port = 25;

    harness.checkPoint("params");
    Socket sock = null;

    try {
      sock = new Socket(host, port);
      
      harness.check(sock.getLocalPort() > 0,
		    "Error : test_params failed - 1 " +
		    "get port did not return proper values");
      
      try {
	sock.setSoTimeout(100);
	harness.check(sock.getSoTimeout() == 100,
		      "Error : test_params failed - 2 " +
		      "get /set timeout did not return proper values");
	harness.check(true);
      } 
      catch (Exception e) {
	harness.check(false, "Error : setSoTimeout fails since some OSes do not support the feature");
	harness.debug(e);
      }

      harness.debug ("getTcpNoDelay() default: " + sock.getTcpNoDelay ());
      harness.check ((sock.getTcpNoDelay () == false), "default getTcpNoDelay() should be false");
      sock.setTcpNoDelay(true);
      harness.check(sock.getTcpNoDelay(),
		    "Error : test_params failed - 3 " +
		    "get /set tcp delay did not return proper values");
      
      harness.debug ("getSoLinger() default: " + sock.getSoLinger());
      harness.check (sock.getSoLinger(), -1, "default getSoLinger() should be -1");
      sock.setSoLinger(true, 10);
      harness.check(sock.getSoLinger() == 10, 
		    "Error : test_params failed - 4");

      sock.setSoLinger(false, 20);
      harness.check(sock.getSoLinger() == -1,
		    "Error : test_params failed - 5");
      
      harness.check(sock.getPort() == port,
		    "Error : test_params failed - 6");
      
      harness.debug("sock.getInetAddress().toString(): " +
		    sock.getInetAddress().toString());
      harness.check(sock.getInetAddress().toString().indexOf(host) != -1,
		    "getInetAddress().toString() should contain host " + host);
      harness.debug("sock.toString(): " + sock.toString());
      harness.check(sock.toString().indexOf(host) != -1,
		    "toString() should contain host " + host);

    }
    catch (Exception e) {
      harness.fail("Error : test_params failed - 10 exception was thrown.");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch(IOException ignored) {}
    }
  }

  public void test_Basics()
  {
    harness.checkPoint("Basics");
    Socket s = null;
    // host name given
    try {
      s = new Socket ("babuspdjflks.gnu.org.", 200);
      harness.fail("Error : test_Basics failed - 1 " +
		   "exception should have been thrown here");
    }
    catch (UnknownHostException e) {
      harness.check(true);
    }
    catch (IOException e) {
      harness.fail("Error : test_Basics failed - 2 " +
		   "Unknown host exception should have been thrown here.");
      harness.debug(e);
    }
    finally {
      try {
	if (s != null)
	  s.close();
      } catch(IOException ignored) {}
    }

    try {
      s = new Socket("127.0.0.1", 30001);
      harness.fail("Error : test_Basics failed - 3 " +
		   "exception should have been thrown here");
    }
    catch (UnknownHostException e) {
      harness.fail("Error : test_Basics failed - 4 " +
		   "Unknown host exception should not have been thrown here");
      harness.debug(e);
    }
    catch (ConnectException e) {
      harness.check(true);      
    }
    catch (IOException e) {
      harness.fail("Error : test_Basics failed - 4 " +
                   "ConnectException should have been thrown here");
      harness.debug(e);
    }
    finally {
      try {
	if (s != null)
	  s.close();
      } catch(IOException ignored) {}
    }

    try {
      s = new Socket("127.0.0.1", 30001, true);
      harness.fail("Error : test_Basics failed - 5 " +
		   "exception should have been thrown here");

    }
    catch (UnknownHostException e) {
      harness.fail("Error : test_Basics failed - 6 " +
		   "Unknown host exception should not have been thrown here");
      harness.debug(e);
    }
    catch (ConnectException e) {
      harness.check(true);
    }
    catch (IOException e) {
      harness.fail("Error : test_Basics failed - 6 " +
                   "ConnectException should have been thrown here");
      harness.debug(e);
    }
    finally {
      try {
	if (s != null)
	  s.close();
      } catch(IOException ignored) {}
    }

    // host inet given
    try {
      // This is host / port that is unlikely to be blocked.  (Outgoing
      // port 80 connections are often blocked.)
      s = new Socket (harness.getMailHost(), 25);
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_Basics failed - 7 " +
		   "exception should not have been thrown.");
      harness.debug(e);
    }
    finally {
      try {
	if (s != null)
	  s.close();
      } catch(IOException ignored) {}
    }

    try {
      s = new Socket(InetAddress.getLocalHost(), 30002);
      harness.fail("Error : test_Basics failed - 8 " +
		   "exception should have been thrown here");
    }
    catch (ConnectException e) {
      harness.check(true);
    }
    catch (IOException e) {
      harness.fail("Error : test_Basics failed - 8 " +
                   "ConnectException should have been thrown here");
      harness.debug(e);
    }
    finally {
      try {
	if (s != null)
	  s.close();
      } catch(IOException ignored) {}
    }

    if (true) { // 1.1 features not implemented
      
      // src socket target socket given(as hostname).
      try {
	s = new Socket ("babuspdjflks.gnu.org.", 200,
			InetAddress.getLocalHost() ,20006);
	harness.fail("Error : test_Basics failed - 9 " +
		     " exception should have been thrown here");
      }
      catch (UnknownHostException e) {
	harness.check(true);
      }
      catch (IOException e) {
	harness.fail("Error : test_Basics failed - 10 " +
		     "UnknownHostException should have been thrown here");
	harness.debug(e);
      }
      finally {
	try {
	  if (s != null)
	    s.close();
	} catch(IOException ignored) {}
      }
      
      try {
	s = new Socket("127.0.0.1", 30003,
		       InetAddress.getLocalHost(), 20007);
	harness.fail("Error : test_Basics failed - 11 " +
		     " exception should have been thrown here");
      }
      catch (UnknownHostException e) {
	harness.fail("Error : test_Basics failed - 12 " +
		     "UnknownHostException should not have been thrown");
	harness.debug(e);
      }
      catch (IOException e) {
	harness.check(true);
      }
      finally {
	try {
	  if (s != null)
	    s.close();
	} catch(IOException ignored) {}
      }
      
      // src socket target socket given (as ip address).
      try {
	s = new Socket(InetAddress.getLocalHost(), 30004,
		       InetAddress.getLocalHost(), 20008);
	harness.fail("Error : test_Basics failed - 13 " +
		     " exception should have been thrown here");
      }
      catch (UnknownHostException e) {
	harness.fail("Error : test_Basics failed - 14 " +
		     "Unknown host exception should not have been thrown");
	harness.debug(e);
      }
      catch (IOException e) {
	harness.check(true);
      }
      finally {
	try {
	  if (s != null)
	    s.close();
	} catch(IOException ignored) {}
      }
    }
  }

  public void test_BasicBServer()
  {
    harness.checkPoint("BasicBServer");
    SocketBServer srv = new SocketBServer();
    SocketBServer.harness = harness;
    srv.init();
    srv.start();
    Thread.yield();

    Socket sock = null;
    try {
      sock = new Socket("127.0.0.1", 20002);
      InputStream is = sock.getInputStream();

      DataInputStream dis = new DataInputStream(is);

      String str = dis.readLine();

      harness.check(str.equals("hello buddy"),
		    "Error : test_BasicServer failed - 1 " +
		    "string returned is not correct.");
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 2 exception was thrown");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch (IOException ignored) {}
    }
  }

  public void test_closed()
  {
    harness.checkPoint("closed");
    Socket sock = null;
    
    try
      {
	sock = new Socket();
	sock.close();
	sock.setSoTimeout(1000);
	harness.fail("exception expected");
      }
    catch (SocketException e)
      {
	harness.check(e.getMessage().compareToIgnoreCase("socket is closed") == 0,
		      "wrong SocketException error message: " + e.getMessage());
      }
    catch (Exception e)
      {
	harness.fail("wrong exception thrown");
      }
  }


  public void testall()
  {
    test_Basics();
    test_params();
    test_BasicServer();
    test_BasicBServer();
    test_closed();
  }

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }
}
