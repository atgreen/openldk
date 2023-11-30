/*************************************************************************
/* ClobTest.java - Test java.sql.Clob
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

package gnu.testlet.java.sql.Clob;

import java.sql.*;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class ClobTest implements Clob, Testlet
{

public void
test(TestHarness harness)
{
  harness.check(true, "java.sql.Clob");
}

public long
length() throws SQLException
{
  return(0);
}

public void
free() throws SQLException
{
}

public Reader 
getCharacterStream (long a, long b) throws SQLException
{
  return(null);
}

public String
getSubString(long offset, int length) throws SQLException
{
  return(null);
}

public InputStream
getAsciiStream() throws SQLException
{
  return(null);
}

public Reader
getCharacterStream() throws SQLException
{
  return(null);
}

public long
position(String pattern, long offset) throws SQLException
{
  return(0);
}

public long
position(Clob pattern, long offset) throws SQLException
{
  return(0);
}

public int
setString(long l, String s) throws SQLException
{
  return(0);
}

public int
setString(long l, String s, int i1, int i2) throws SQLException
{
  return(0);
}

public OutputStream setAsciiStream(long l) throws SQLException
{
  return(null);
}

public Writer
setCharacterStream(long l) throws SQLException
{
  return(null);
}

public void
truncate(long l) throws SQLException
{
  return;
}

} // class ClobTest
