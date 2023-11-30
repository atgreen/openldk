// Copyright (C) 2007 Red Hat, Inc.

// Tags: JDK1.5

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

package gnu.testlet.java.lang.ProcessBuilder;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.IOException;

public class simple implements Testlet
{
  public void test(TestHarness harness)
  {
    try
      {
	ProcessBuilder p1 = new ProcessBuilder("ls", "/nosuchdirectory");
	p1.redirectErrorStream(true);
	Process p = p1.start();
	byte[] buffer = new byte[1024];
	harness.check(p.getInputStream().read(buffer) != -1);
      }
    catch (IOException _)
      {
	harness.check(false);
      }	
  }
}
