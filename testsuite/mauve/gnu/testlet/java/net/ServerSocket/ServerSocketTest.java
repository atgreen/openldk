// Tags: JDK1.0
// Uses: BasicBacklogSocketServer BasicSocketServer MyBasicSocketServer MyServerSocket

/*
   Copyright (C) 1999 Hewlett-Packard Company

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

package gnu.testlet.java.net.ServerSocket;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;
import java.io.*;


public class ServerSocketTest implements Testlet
{
  
  protected static TestHarness harness;

  private static final boolean USE_JOIN = false;

  /**
   * Wait (for up to 5 seconds) until a thread has completed.  If
   * 'USE_JOIN' is true, use join.  Otherwise use sleep and isAlive.
   * @param thread the thread to wait for,
   * @return true is the thread ended, false otherwise.
   */
  private boolean completed(Thread thread)
  {
    try {
      if (USE_JOIN) {
	thread.join(5000);
      }
      else {
	for (int i = 0; i < 5 && thread.isAlive(); i++) {
	  Thread.sleep(1000);
	}
      }
    }
    catch (Exception e) { /* Squelch exceptions */ }
    return !thread.isAlive();
  }
  
  public void test_BasicBacklogServer()
  {
    BasicBacklogSocketServer srv = new BasicBacklogSocketServer();
    srv.init(harness);
    srv.start();
    Thread.yield();
    
    try {
      Socket sock = new Socket("127.0.0.1", 21000);
      DataInputStream dis = new DataInputStream(sock.getInputStream());
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicBacklogServer failed - 2 " + 
		   "exception was thrown " + e);
    }
    
    // second iteration
    try {
      Socket sock = new Socket("127.0.0.1", 21000);
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicBacklogServer failed - 3 " + 
		   "exception was thrown " + e);
    }
    
    // third iteration
    try {
      Socket sock = new Socket("127.0.0.1", 21000);
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicBacklogServer failed - 4 " + 
		   "exception was thrown " + e);
    }
    
    harness.check(completed(srv), 
		  "Error : test_BasicBacklogServer failed - 5 " + 
		  "server didn't end");
  }
  
  public void test_BasicServer()
  {
    harness.checkPoint("BasicServer");
    BasicSocketServer srv = new BasicSocketServer();
    srv.init(harness);
    srv.start();
    Thread.yield();
    
    try {
      Socket sock = new Socket("127.0.0.1", 12000);
      DataInputStream dis = new DataInputStream(sock.getInputStream());
      String str = dis.readLine();
      
      harness.check(str.equals("hello buddy"),
		    "Error : test_BasicServer failed - 1 " + 
		    "string returned is not correct.");
      sock.close();
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_BasicServer failed - 2 " + 
		   "exception was thrown: " + e.getMessage());
    }
    
    // second iteration
    try {
      Socket sock = new Socket("127.0.0.1", 12000);
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
		   "exception was thrown: " + e.getMessage());
    }
    
    if (!completed(srv)) {
      harness.fail("Error : test_BasicServer failed - 5 " + 
		   " server didn't end ");
      // Attempt to clean up the server thread by 1) closing it's
      // socket and 2) interrupting it.
      try {
	srv.srvsock.close();
      } catch (Exception e) {
	harness.fail("Error : test_BasicServer failed - 6 " + 
		     " exception in close: " + e.getMessage());
      }
      
      if (!completed(srv)) {
	harness.fail("Error : test_BasicServer failed - 7 " + 
		     "server didn't end when its socket was closed");
	// Try to unjam it ...
	try {
	  srv.interrupt();
	}
	catch (Exception e) {
	  harness.fail("Error : test_BasicServer failed - 8 " + 
		       " exception in interrupt: " + e.getMessage());
	}
	if (!completed(srv)) {
	  // The server thread is still alive.  Oh dear ...
	  harness.fail("Error : test_BasicServer failed - 9 " + 
		       "server didn't end when interrupted");
	}
      }
    }
    else {
      harness.check(true);
    }
  }
  
  public void test_MyBasicServer()
  {
    MyBasicSocketServer srv = new MyBasicSocketServer();
    srv.init(harness);
    srv.start();
    Thread.yield();
    try {
      Socket sock = new Socket("127.0.0.1", 12000);
    } catch (IOException e) {}
  }
  
  public void test_params()
  {
    ServerSocket sock = null;
    try {
      sock = new ServerSocket(30000);
      
      harness.check(sock.getLocalPort() == 30000,
		    "Error : test_params failed - 1 " + 
		    "get port did not return proper values");
      
      if (false) {  // set/getSoTimeout not there
	try {
	  sock.setSoTimeout(100);
	  if (sock.getSoTimeout() != 100) {
	    harness.fail("Error : test_params failed - 2 " + 
			 "get /set timeout did not return proper values");
	  }
	}
	catch (IOException e) {
	  harness.fail("Error : setSoTimeout fails since vxWorks do " +
		       "not support the feature");
	  harness.debug(e);
	}
      }
      
      try {
	ServerSocket sock1 = new ServerSocket(30000);
	harness.fail("Error : test_params failed - 3 " + 
		     "should have thrown bind exception here.");
      }
      catch (IOException e) { // Should this be more specific?
	harness.check(true);
      }
      
      String ip = "0.0.0.0";
      harness.check(sock.toString().indexOf(ip) != -1,
		    "toString() should contain IP");
      InetAddress addr = sock.getInetAddress();
      if (addr == null) {
	harness.fail("Error : test_params failed - 4 " +
		     "sock.getInetAddress() -> null");
      }
      else {
	harness.check(addr.toString().indexOf(ip) != -1,
		      "InetAddress toString() should contain IP");
      }
      ServerSocket.setSocketFactory(null);
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error : test_params failed - 10 " + 
		   "exception was thrown");
      harness.debug(e);
    }
    finally {
      try {
	if (sock != null)
	  sock.close();
      } catch (IOException ignored) {}
    }
    
  }

  public void test_close()
  {
    try {
      harness.checkPoint("test_close");
      ServerSocket s = new ServerSocket(0);
      harness.check(s.isBound());
      harness.check(!s.isClosed());
      int port = s.getLocalPort();
      InetAddress address = s.getInetAddress();
      SocketAddress sockAddress = s.getLocalSocketAddress();
      s.close();
      harness.check(s.isBound());
      harness.check(s.isClosed());
      harness.check(port == s.getLocalPort());
      harness.check(address.equals(s.getInetAddress()));
      harness.check(sockAddress.equals(s.getLocalSocketAddress()));
    } catch (Exception e) {
      harness.fail("Unexpected exception");
      harness.debug(e);
    }
  }
  
  public void testall()
  {
    test_BasicServer();
    test_MyBasicServer();
    test_BasicBacklogServer();
    test_params();
    test_close();
  }
  
  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall();
  }

}
