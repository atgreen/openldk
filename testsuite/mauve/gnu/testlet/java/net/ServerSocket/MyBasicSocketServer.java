// Tags: not-a-test

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
import gnu.testlet.TestHarness;
import java.net.*;
import java.io.*;


class MyBasicSocketServer extends Thread {
  MyServerSocket srvsock = null;
  
  private TestHarness harness;
  
  public void init(TestHarness harness)
  {
    this.harness = harness;
    try {
      srvsock = new MyServerSocket(10000);
      srvsock.close();
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error - 1 : MyBasicSocketServer::init failed " + 
		   "exception in new MyServerSocket(10000): " + e);
    }
    // now do the real one
    try {
      srvsock = new MyServerSocket(20000);
      harness.check(true);
    }
    catch (Exception e) {
      harness.fail("Error - 2 : MyBasicSocketServer::init failed " + 
		   "exception in new MyServerSocket(20000): " + e);
    }
  }
  
  public void run()
  {
    try {
      Socket clnt = new Socket("localhost", 20000);
      srvsock.invoke_implAccept(clnt);
    } catch (IOException e) {}
    // get finalize to be invoked
    try {
      srvsock.finalize();
    }
    catch (Exception e) {}
  }
}
