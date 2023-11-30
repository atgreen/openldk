/*************************************************************************
/* BlobTest.java - Test java.util.Blob interface
/*
/* Copyright (c) 1999 Aaron M. Renn (arenn@urbanophile.com)
/* Copyright (c) 2002 Mark J. Wielaard (mark@klomp.org)
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

// Tags: JDK1.2 JDBC2.0

package gnu.testlet.java.sql.Blob;

import java.sql.*;
import java.io.InputStream;
import java.io.OutputStream;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class BlobTest implements Blob, Testlet
{

public void
test(TestHarness harness)
{
  harness.check(true, "java.sql.Blob");
}

public void
free() throws SQLException
{
}

public InputStream 
getBinaryStream (long a, long b) throws SQLException
{
  return(null);
}

public long
length() throws SQLException
{
  return(0);
}

public byte[]
getBytes(long offset, int length) throws SQLException
{
  return(null);
}

public InputStream
getBinaryStream() throws SQLException
{
  return(null);
}

public long
position(byte[] pattern, long offset) throws SQLException
{
  return(0);
}

public long
position(Blob pattern, long offset) throws SQLException
{
  return(0);
}

public int
setBytes(long l,byte[] bs)
{
  return(0);
}

public int
setBytes(long l ,byte[] bs ,int i1, int i2)
{
  return(0);
}

public void
truncate(long l)
{
  return;
}

public OutputStream setBinaryStream(long l)
{
  return(null);
}

} // class BlobTest

