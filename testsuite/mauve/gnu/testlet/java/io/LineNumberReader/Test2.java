// ------------------------------------------------------------------------
// Test2.java -- Tests LineNumberReader
//
// Copyright (c) 2003 Free Software Foundation, Inc.
// Written by Guilhem Lavaux <guilhem@kaffe.org>, Based on a test by
// Dalibor Topic <robilad@kaffe.org>
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published 
// by the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
// ------------------------------------------------------------------------
// Tags: JDK1.1

package gnu.testlet.java.io.LineNumberReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class Test2 implements Testlet
{
  static abstract class LineReaderTest
  {
    abstract void test(TestHarness harness) throws Exception;
  }

  static class LineTest1 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("X");
      LineNumberReader lnr = new LineNumberReader(sr);

      try
	{
	  lnr.mark(-5);
	  harness.check(false);
	}
      catch (IllegalArgumentException e)
	{
	  harness.check(true);
	}
    }
  }

  static class LineTest2 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("X");
      LineNumberReader lnr = new LineNumberReader(sr);

      try
	{
	  lnr.read(null, 0, 0);
	  harness.check(false);
	}
      catch (NullPointerException e)
	{
	  harness.check(true);
	}

      // Read too many bytes for the buffer.
      try
	{
	  lnr.read(new char[1], 0, 2);
	  harness.check(false);
	}
      catch (IndexOutOfBoundsException e)
	{
	  harness.check(true);
	}

      // Read at a negative position.
      try
	{
	  lnr.read(new char[1], -5, 0);
	  harness.check(false);
	}
      catch (IndexOutOfBoundsException e)
	{
	  harness.check(true);
	}
 
      // Read with a negative length.
      try
	{
	  lnr.read(new char[1], 0, -5);
	  harness.check(false);
	}
      catch (IndexOutOfBoundsException e)
	{
	  harness.check(true);
	}
    }
  }

  static class LineTest3 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("X");
      LineNumberReader lnr = new LineNumberReader(sr);

      lnr.setLineNumber(-5);
      harness.check(lnr.getLineNumber(), -5);
    }
  }
  
  static class LineTest4 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("\r\n");
      LineNumberReader lnr = new LineNumberReader(sr);

      char[] ch = new char[2];
      int r = lnr.read(ch, 0, 2);
      harness.check(ch[0] == '\r' && ch[1] == '\n');
      harness.check(lnr.getLineNumber(), 1);
    }
  }

  static class LineTest5 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("\r\n\r");
      LineNumberReader lnr = new LineNumberReader(sr);

      harness.check(lnr.read(), '\n');
      harness.check(lnr.read(), '\n');
      harness.check(lnr.getLineNumber(), 2);
    }
  }

  static class LineTest6 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("\r\r\n");
      LineNumberReader lnr = new LineNumberReader(sr);

      harness.check(lnr.read(), '\n');
      harness.check(lnr.read(), '\n');
      harness.check(lnr.getLineNumber(), 2);
    }
  }

  static class LineTest7 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("\r\n\r");
      LineNumberReader lnr = new LineNumberReader(sr);
      char[] ch = new char[1];

      harness.check(lnr.read(), '\n');
      harness.check(lnr.read(ch, 0, 1), 1);
      harness.check(ch[0], '\n');
      harness.check(lnr.read(), '\n');
      harness.check(lnr.getLineNumber(), 2);
    }
  }

  static class LineTest8 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("\r\n\r");
      LineNumberReader lnr = new LineNumberReader(sr);
      char[] ch = new char[1];

      harness.check(lnr.read(ch, 0, 1), 1);
      harness.check(ch[0], '\r');
      harness.check(lnr.read(), '\n');
      harness.check(lnr.read(ch, 0, 1), -1);
      harness.check(ch[0], '\r');
      harness.check(lnr.getLineNumber(), 2);
    }
  }


  static class LineTest9 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("\r\n\r");
      LineNumberReader lnr = new LineNumberReader(sr);
      char[] ch = new char[1];

      lnr.read();
      lnr.mark(5);
      harness.check(lnr.read(ch, 0, 1), 1);
      harness.check(ch[0], '\n');
      harness.check(lnr.read(), '\n');
      lnr.reset();
      harness.check(lnr.read(ch, 0, 1), 1);
      harness.check(ch[0], '\n');
      harness.check(lnr.read(), '\n');
      
      harness.check(lnr.getLineNumber(), 2);
    }    
  }

  static class LineTest10 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("X");
      LineNumberReader lnr = new LineNumberReader(sr);
     
      try
	{
	  lnr.reset();
	  harness.check(false);
	}
      catch (IOException e)
	{
	  harness.check(true);
	}
    }
  }

  static class LineTest11 extends LineReaderTest
  {
    void test(TestHarness harness) throws Exception
    {
      StringReader sr = new StringReader("X");
      LineNumberReader lnr = new LineNumberReader(sr);
      int old_linenumber = lnr.getLineNumber();
      
      lnr.mark(5);
      lnr.setLineNumber(10);
      lnr.reset();
      harness.check(lnr.getLineNumber(), old_linenumber);
    }
  }

  static LineReaderTest[] tests = {
    new LineTest1(),
    new LineTest2(),
    new LineTest3(),
    new LineTest4(),
    new LineTest5(),
    new LineTest6(),
    new LineTest7(),
    new LineTest8(),
    new LineTest9(),
    new LineTest10(),
    new LineTest11()
  };

  public void test(TestHarness harness)
  {
    for (int i = 0; i < tests.length; i++)
      {
	String name = tests[i].getClass().getName();

	name = name.substring(name.indexOf('$')+1);
	harness.checkPoint("LineNumberReader stress test (" + name + ")");
	try
	  {
	    tests[i].test(harness);
	  }
	catch (Exception e)
	  {
	    harness.check(false);
	    harness.debug(e);
	  }
      }
  }  
}
