/*************************************************************************
/* ReadReference2.java -- Tests Data{Input,Output}Stream's
/*
/* Copyright (c) 1998, 1999 Free Software Foundation, Inc.
/* Written by Daryl Lee (dol@sources.redhat.com)
/* Shameless ripoff of ReadReference.java by  Aaron M. Renn (arenn@urbanophile.com)
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
// Uses: ReadStream2

package gnu.testlet.java.io.DataInputStream;

import java.io.*;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;


public class ReadReference2 implements Testlet
{

public void
test(TestHarness harness)
{
  try
    {
      InputStream is = harness.getResourceStream(
           "gnu#testlet#java#io#DataInputStream#reference2.data");
      DataInputStream dis = new DataInputStream(is); 

      harness.debug("Reading reference DataInput data, set 2");
      ReadStream2.runReadTest(dis, harness);

      dis.close();
    }
  catch (Exception e)
    {
      harness.debug(e);
      harness.check(false, "Read reference DataInput data (2)");
    }
}

} // class ReadReference2

