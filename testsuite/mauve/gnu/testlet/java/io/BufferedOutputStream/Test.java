/*************************************************************************
/* Test.java -- Test (Buffered,ByteArray)OutputStream
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
/* Modified by Daryl Lee (dolee@sources.redhat.com)
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

package gnu.testlet.java.io.BufferedOutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class Test implements Testlet
{

public void 
test(TestHarness harness)
{

  try
    {
      ByteArrayOutputStream baos = new ByteArrayOutputStream(24);
      BufferedOutputStream bos = new BufferedOutputStream(baos, 12);

      String str = "The Kroger on College Mall Rd. in Bloomington\n" +
        "used to sell Kroger brand frozen pizzas for 68 cents.\n" +
        "I ate a lot of those in college.  It was kind of embarrassing\n" +
        "walking out of the grocery with nothing but 15 frozen pizzas.\n";

      boolean passed = true;

      byte[] buf = str.getBytes();
      bos.write(buf, 0, 5);
      harness.check(baos.toByteArray().length, 0, "buffering/toByteArray");
      bos.write(buf, 5, 8);
      bos.write(buf, 13, 12);
      bos.write(buf[25]);
      bos.write(buf, 26, buf.length - 26);
      bos.close();

      String str2 = new String(baos.toByteArray());
      harness.check(str, str2, "did all bytes come through?");
      harness.debug(str2, false);
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // class Test

