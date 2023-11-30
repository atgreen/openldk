/*************************************************************************
/* TestJdbc20.java -- Test java.sql.Types for JDK 1.2/JDBC 2.0
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

// Tags: JDK1.2 JDBC2.0

package gnu.testlet.java.sql.Types;

import java.sql.Types;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class TestJdbc20 implements Testlet
{

public void 
test(TestHarness harness)
{
  harness.check(Types.JAVA_OBJECT, 2000, "JAVA_OBJECT");
  harness.check(Types.DISTINCT, 2001, "DISTINCT");
  harness.check(Types.STRUCT, 2002, "STRUCT");
  harness.check(Types.ARRAY, 2003, "ARRAY");
  harness.check(Types.BLOB, 2004, "BLOB");
  harness.check(Types.CLOB, 2005, "CLOB");
  harness.check(Types.REF, 2006, "REF");
}

} // class TestJdbc20

