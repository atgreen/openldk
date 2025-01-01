/*************************************************************************
/* ArrayTest.java - Test java.sql.Array interface
/*
/* Copyright (c) 1999 Aaron M. Renn (arenn@urbanophile.com)
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

package gnu.testlet.java.sql.Array;

import java.sql.*;
import java.util.Map;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class ArrayTest implements Array, Testlet
{

public void
test(TestHarness harness)
{
  harness.check(true, "java.sql.Array");
}

public String
getBaseTypeName() throws SQLException
{
  return(null);
}

public void
free() throws SQLException
{
}

public int
getBaseType() throws SQLException
{
  return(0);
}

public Object
getArray() throws SQLException
{
  return(null);
}

public Object
getArray(Map map) throws SQLException
{
  return(null);
}

public Object
getArray(long offset, int count) throws SQLException
{
  return(null);
}

public Object
getArray(long index, int count, Map map) throws SQLException
{
  return(null);
}

public ResultSet
getResultSet() throws SQLException
{
  return(null);
}

public ResultSet
getResultSet(Map map) throws SQLException
{
  return(null);
}

public ResultSet
getResultSet(long index, int count) throws SQLException
{
  return(null);
}

public ResultSet
getResultSet(long index, int count, Map map) throws SQLException
{
  return(null);
}

} // class ArrayTest

