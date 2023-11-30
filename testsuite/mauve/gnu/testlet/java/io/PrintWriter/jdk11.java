/*************************************************************************
/* Test.java -- Tests PrintWriter
/*
/* Copyright (c) 1998, 2003 Free Software Foundation, Inc.
/* Written by Daryl Lee (dolee@sources.redhat.com)
/*
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

package gnu.testlet.java.io.PrintWriter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.PrintWriter;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.CharArrayWriter;

public class jdk11 extends PrintWriter implements Testlet
{

public jdk11()
{
  this(new ByteArrayOutputStream());
}

jdk11(OutputStream os)
{
  super(os);
}

public void print(int i, boolean err)
{
  if (err)
    {
      this.setError();
    }
}

public String toString()
{
  return ("ObjectString");
}

public void
test(TestHarness harness)
{
// Test constructors first
	ByteArrayOutputStream baos1 = new ByteArrayOutputStream();
	PrintWriter pw1 = new PrintWriter(baos1);
	harness.check(pw1 != null, "PrintWriter(OutputStream)");
	ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
	PrintWriter pw2 = new PrintWriter(baos2, true);
	harness.check(pw2 != null, "PrintWriter(OutputStream, boolean)");
	CharArrayWriter caw1 = new CharArrayWriter();
	PrintWriter pw3 = new PrintWriter(caw1);
	harness.check(pw3 != null, "PrintWriter(Writer)");
	CharArrayWriter caw2 = new CharArrayWriter();
	PrintWriter pw4 = new PrintWriter(caw2);
	harness.check(pw4 != null, "PrintWriter(Writer)");
// Now test the methods
	pw1.print(true);
	pw1.print(false);
	pw1.print('X');
	char[] ca = {'A', 'B', 'C'};
	pw1.print(ca);
	double x = 3.14;
	pw1.print(x);
	float y = (float) 1.414;
	pw1.print(y);
	int i = 37;
	pw1.print(i);
	long l = 65537L;
	pw1.print(l);
	pw1.print(new jdk11());
	pw1.print("XYZ");
	pw1.write(ca);
	pw1.write(ca,0,2);
	pw1.write('Q');
	pw1.write("JKL");
	pw1.write("MNOPQ", 1, 2);
	pw1.println();
	pw1.println(true);
	pw1.println(false);
	pw1.println('X');
	pw1.println(ca);
	pw1.println(x);
	pw1.println(y);
	pw1.println(i);
	pw1.println(l);
	pw1.println(new jdk11());
	pw1.println("XYZ");
	
	pw1.flush();
	harness.check(true, "flush()");
	pw1.close();
	harness.check(true, "close()");
	String tst = "truefalseXABC3.141.4143765537ObjectStringXYZABCABQJKLNO\n" +
					"true\nfalse\nX\nABC\n3.14\n1.414\n37\n65537\nObjectString\nXYZ\n";
	harness.check(baos1.toString(), tst, "All characters printed okay");
	harness.debug("Final output:" + baos1.toString());

	// Set up to test setError() and checkError()	
	ByteArrayOutputStream baos3 = new ByteArrayOutputStream();
	jdk11 tpw = new jdk11(baos3);
	harness.check(!tpw.checkError(), "checkError");
	tpw.print(3, true);   // forces call to setError
	harness.check(tpw.checkError(), "setError");

	// Check for (no) error after close
	PrintWriter p = new PrintWriter(baos3);
	p.close();
	harness.check(!p.checkError(), "checkError() after close()");
}

} // class Test

