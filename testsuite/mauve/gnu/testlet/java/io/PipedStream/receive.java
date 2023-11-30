// Tags: JDK1.0

// Test PipedInputStream.receive().
// Written by Tom Tromey <tromey@cygnus.com>

// Copyright (C) 2000 Red Hat, Inc.

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.io.PipedStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class receive extends PipedInputStream implements Runnable, Testlet
{
  static Thread main;
  static receive in;
  static PipedOutputStream out;

  receive (PipedOutputStream x) throws IOException
  {
    super(x);
  }

  public receive ()
  {
  }

  public void run() {
    try {
      Thread.sleep(1000);
      in.receive(23);
    } catch (Throwable t) {
    }
  }

  public void test (TestHarness harness) {
    int val = -1;
    try {
      main = Thread.currentThread();
      out = new PipedOutputStream();
      in = new receive (out);

      (new Thread(in)).start();

      val = in.read();
    } catch (Throwable t) {
      val = -2;
    }
    harness.check (val, 23);
  }
}
