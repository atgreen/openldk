/*
 * Based on Kaffe's Connection
 *
 * Copyright (c) 1998
 *      Transvirtual Technologies, Inc.  All rights reserved.
 * Modifications Copyright (c) 1999 Aaron M. Renn (arenn@urbanophile.com) 
 * Modifications Copyright (c) 2002 Mark J. Wielaard (mark@klomp.org) 
 *
 * See the file "COPYING" for information on usage and redistribution
 * of this file.
 */

// Tags: JDK1.1 JDBC1.0

package gnu.testlet.java.sql.Connection;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.sql.*;

public class TestJdbc implements Testlet {

public void
test(TestHarness harness)
{
  harness.check(Connection.TRANSACTION_NONE, 0,
		"TRANSACTION_NONE");
  harness.check(Connection.TRANSACTION_READ_UNCOMMITTED, 1, 
		"TRANSACTION_READ_UNCOMMITTED");
  harness.check(Connection.TRANSACTION_READ_COMMITTED, 2,
		"TRANSACTION_READ_COMMITTED");
  harness.check(Connection.TRANSACTION_REPEATABLE_READ, 4,
		"TRANSACTION_REPEATABLE_READ");
  harness.check(Connection.TRANSACTION_SERIALIZABLE, 8,
		"TRANSACTION_SERIALIZABLE");
}

}
