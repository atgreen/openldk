/*************************************************************************
/* SimpleRead.java -- ByteArrayInputStream simple read test
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

package gnu.testlet.java.io.ByteArrayInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class SimpleRead implements Testlet
{

public void
test(TestHarness harness)
{
  String str = "My sophomore year of college I moved out of the dorms. I\n" +
     "moved in with three friends into a brand new townhouse in east\n" +
     "Bloomington at 771 Woodbridge Drive.  To this day that was the\n" +
     "nicest place I've ever lived.\n";

  byte[] str_bytes = str.getBytes();  
  ByteArrayInputStream bais = new ByteArrayInputStream(str_bytes);
  byte[] read_buf = new byte[12];

  try
    {
      int bytes_read, total_read = 0;
      while ((bytes_read = bais.read(read_buf, 0, read_buf.length)) != -1)
        {
          harness.debug(new String(read_buf, 0, bytes_read), false);
          total_read += bytes_read;
        }

      bais.close();
      harness.check(total_read, str.length(), "total_read");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // SimpleRead

