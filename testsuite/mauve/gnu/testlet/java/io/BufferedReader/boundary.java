// Test of a boundary case in BufferedReader

/*************************************************************************
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.1

package gnu.testlet.java.io.BufferedReader;

import java.io.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class boundary implements Testlet
{
  public void test (TestHarness harness)
  {
    try
      {
	// This test comes from gcj PR 6301.
	String str = "abcd\r\nefghijklm\r\n";
	StringReader sr = new StringReader(str);
	// `5' here makes the buffer stop between the \r and the \n.
	BufferedReader br = new BufferedReader(sr, 5);

	String l1 = br.readLine();
	harness.check(l1, "abcd");

	br.mark(1);
	char c = (char) br.read();
	harness.check(c, 'e');
	br.reset();

	// The libgcj/Classpath bug is that BufferedReader gets confused
	// and returns "" here.
	String l2 = br.readLine();
	harness.check(l2, "efghijklm");

	// check ready() and skip()
	sr = new StringReader("efghijklm\n");
	br = new BufferedReader(sr);
	harness.check(br.ready(), "ready()");
	br.skip(2L);
	l2 = br.readLine();
	harness.check(l2, "ghijklm");
      }
    catch (IOException e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }
}
