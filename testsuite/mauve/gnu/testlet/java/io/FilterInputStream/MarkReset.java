/*************************************************************************
/* MarkReset.java -- FilterInputStream mark/reset test
/*
/* Copyright (c) 1998, 1999 Free Software Foundation, Inc.
/* Written for ByteArrayInputStream by Aaron M. Renn (arenn@urbanophile.com)
/* Adapted to FilterInputStream by Daryl O. Lee (dolee@sources.redhat.com)
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

package gnu.testlet.java.io.FilterInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.ByteArrayInputStream;
import java.io.FilterInputStream;
import java.io.IOException;

public class MarkReset extends FilterInputStream implements Testlet
{

public MarkReset()
{
  this(null);
}

public MarkReset(ByteArrayInputStream is)
{
  super(is);
}

public void
test(TestHarness harness)
{
  String str = "My sophomore year of college I moved out of the dorms. I\n" +
     "moved in with three friends into a brand new townhouse in east\n" +
     "Bloomington at 771 Woodbridge Drive.  To this day that was the\n" +
     "nicest place I've ever lived.\n";

  byte[] str_bytes = str.getBytes();  
  ByteArrayInputStream bais = new ByteArrayInputStream(str_bytes);
  MarkReset fis = new MarkReset(bais);
  byte[] read_buf = new byte[12];

  try
    {
      fis.read(read_buf);      

      harness.check(fis.available(), (str_bytes.length - read_buf.length),
                    "available() 1");
      harness.check(fis.skip(5), 5, "skip()");
      // System.out.println("skip() didn't work");
      harness.check(fis.available(), (str_bytes.length - 
                   (read_buf.length + 5)), "available() 2");
      harness.check(fis.markSupported(), "markSupported()");

      fis.mark(0);
      int availsave = fis.available();
      fis.read();
      fis.reset();
      harness.check(fis.available(), availsave, "mark(),reset()");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // MarkReset

