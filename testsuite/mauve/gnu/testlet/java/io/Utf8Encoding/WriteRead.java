/*************************************************************************
/* WriteRead.java -- A quick test of the UTF8 encoding
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
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

package gnu.testlet.java.io.Utf8Encoding;

import java.io.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class WriteRead implements Testlet
{

public void
test(TestHarness harness)
{
  String str1 = "This is the first line of text\n";
  String str2 = "This has some \u01FF\uA000\u6666\u0200 weird characters\n";

  // First write
  try
    {
      FileOutputStream fos = new FileOutputStream("utf8test.out");
      OutputStreamWriter osr = new OutputStreamWriter(fos, "UTF8");
      osr.write(str1);
      osr.write(str2);
      osr.close();

      harness.check(true, "Write UTF8 test (conditionally)");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false, "Write UTF8 test");
      return;
    }

  // Then read
  try
    {
      FileInputStream fis = new FileInputStream("utf8test.out");
      InputStreamReader isr = new InputStreamReader(fis, "UTF8");
      char[] buf = new char[255];

      int chars_read = isr.read(buf, 0, str1.length());
      String str3 = new String(buf, 0, chars_read);

      chars_read = isr.read(buf, 0, str2.length());
      String str4 = new String(buf, 0, chars_read);

      harness.check(str1, str3, "Read UTF8 file");
      harness.check(str2, str4, "Read UTF8 file");

      isr.close();
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false, "Read UTF8 file");
    }
}

} // class UTF8EncodingTest

