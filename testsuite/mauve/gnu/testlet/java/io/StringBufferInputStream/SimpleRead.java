/*************************************************************************
/* SimpleRead.java -- StringBufferInputStream simple read test
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

// Tags: JDK1.0

package gnu.testlet.java.io.StringBufferInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class SimpleRead implements Testlet
{

public void
test(TestHarness harness)
{
  String str = "Between my freshman and sophomore years of high school\n" +
    "we moved into a brand new building.  The old high school was turned\n" +
    "into an elementary school.\n";

  StringBufferInputStream sbis = new StringBufferInputStream(str);

  try
    {
      int bytes_read, total_read = 0;
      StringBuffer sb = new StringBuffer("");
      byte[] read_buf = new byte[12];
      while ((bytes_read = sbis.read(read_buf, 0, read_buf.length)) != -1)
        {
          sb.append(new String(read_buf, 0, bytes_read));
          total_read += bytes_read;
        }
      harness.debug(sb.toString());

      sbis.close();
      harness.check(total_read, str.length(), "Bytes read");
      harness.check(str, sb.toString(), "String contents");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

}

