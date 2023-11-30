/*************************************************************************
/* ZeroRead.java -- BufferedInputStream zero read test
/*
/* Copyright (c) 2004 Free Software Foundation, Inc.
/* Written by Jeroen Frijters (jeroen@frijters.net)
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

package gnu.testlet.java.io.BufferedInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class ZeroRead extends InputStream implements Testlet
{
    public int read() throws IOException
    {
	throw new IOException();
    }

    public int read(byte[] b, int off, int len) throws IOException
    {
	if (len == 0)
	    return 0;
	throw new IOException();
    }

    public void test(TestHarness harness)
    {
        try
          {
	    // Make sure that a zero length read doesn't result in a read on the
	    // underlying stream.
	    BufferedInputStream bis = new BufferedInputStream(this);
            harness.check(bis.read(new byte[0], 0, 0) == 0);
	  }
        catch (IOException e)
          {
	    harness.debug(e);
	    harness.check(false);
	  }
    }
}
