/*************************************************************************
/* TestJdbc10.java -- Test java.sql.Types for JDK 1.1/JDBC 1.0
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

// Tags: JDK1.1 JDBC1.0 !JDK1.2 !JDBC2.0

package gnu.testlet.java.sql.Types;

import java.sql.Types;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class TestJdbc10 implements Testlet
{

public void 
test(TestHarness harness)
{
  harness.check(Types.BIT, -7, "BIT");
  harness.check(Types.TINYINT, -6, "TINYINT");
  harness.check(Types.SMALLINT, 5, "SMALLINT");
  harness.check(Types.INTEGER, 4, "INTEGER");
  harness.check(Types.BIGINT, -5, "BIGINT");
  harness.check(Types.FLOAT, 6, "FLOAT");
  harness.check(Types.REAL, 7, "REAL");
  harness.check(Types.DOUBLE, 8, "DOUBLE");
  harness.check(Types.NUMERIC, 2, "NUMERIC");
  harness.check(Types.DECIMAL, 3, "DECIMAL");
  harness.check(Types.CHAR, 1, "CHAR");
  harness.check(Types.VARCHAR, 12, "VARCHAR");
  harness.check(Types.LONGVARCHAR, -1, "LONGVARCHAR");
  harness.check(Types.DATE, 91, "DATE");
  harness.check(Types.TIME, 92, "TIME");
  harness.check(Types.TIMESTAMP, 93, "TIMESTAMP");
  harness.check(Types.BINARY, -2, "BINARY");
  harness.check(Types.VARBINARY, -3, "VARBINARY");
  harness.check(Types.LONGVARBINARY, -4, "LONGVARBINARY");
  harness.check(Types.NULL, 0, "NULL");
  harness.check(Types.OTHER, 1111, "OTHER");
}

} // class TestJdbc10

