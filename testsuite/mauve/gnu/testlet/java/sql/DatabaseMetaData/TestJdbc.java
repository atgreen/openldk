/*
 * Test java.sql.DatabaseMetaData
 *
 * Copyright (c) 1998
 *      Transvirtual Technologies, Inc.  All rights reserved.
 * Copyright (c) 1999 Aaron M. Renn (arenn@urbanophile.com)
 * Copyright (c) 2002 Mark J. Wielaard (mark@klomp.org)
 *
 * See the file "COPYING" for information on usage and redistribution
 * of this file.
 */

// Tags: JDK1.1 JDBC1.0

package gnu.testlet.java.sql.DatabaseMetaData;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class TestJdbc implements Testlet
{

public void
test(TestHarness harness)
{
  harness.check(java.sql.DatabaseMetaData.procedureResultUnknown, 0);
  harness.check(java.sql.DatabaseMetaData.procedureNoResult, 1);
  harness.check(java.sql.DatabaseMetaData.procedureReturnsResult, 2);

  harness.check(java.sql.DatabaseMetaData.procedureColumnUnknown, 0);
  harness.check(java.sql.DatabaseMetaData.procedureColumnIn, 1);
  harness.check(java.sql.DatabaseMetaData.procedureColumnInOut, 2);
  harness.check(java.sql.DatabaseMetaData.procedureColumnOut, 4);
  harness.check(java.sql.DatabaseMetaData.procedureColumnReturn, 5);
  harness.check(java.sql.DatabaseMetaData.procedureColumnResult, 3);

  harness.check(java.sql.DatabaseMetaData.procedureNoNulls, 0);
  harness.check(java.sql.DatabaseMetaData.procedureNullable, 1);
  harness.check(java.sql.DatabaseMetaData.procedureNullableUnknown, 2);

  harness.check(java.sql.DatabaseMetaData.columnNoNulls, 0);
  harness.check(java.sql.DatabaseMetaData.columnNullable, 1);
  harness.check(java.sql.DatabaseMetaData.columnNullableUnknown, 2);

  harness.check(java.sql.DatabaseMetaData.bestRowTemporary, 0);
  harness.check(java.sql.DatabaseMetaData.bestRowTransaction, 1);
  harness.check(java.sql.DatabaseMetaData.bestRowSession, 2);

  harness.check(java.sql.DatabaseMetaData.bestRowUnknown, 0);
  harness.check(java.sql.DatabaseMetaData.bestRowNotPseudo, 1);
  harness.check(java.sql.DatabaseMetaData.bestRowPseudo, 2);

  harness.check(java.sql.DatabaseMetaData.versionColumnUnknown, 0);
  harness.check(java.sql.DatabaseMetaData.versionColumnNotPseudo, 1);
  harness.check(java.sql.DatabaseMetaData.versionColumnPseudo, 2);

  harness.check(java.sql.DatabaseMetaData.importedKeyCascade, 0);
  harness.check(java.sql.DatabaseMetaData.importedKeyRestrict, 1);
  harness.check(java.sql.DatabaseMetaData.importedKeySetNull, 2);
  harness.check(java.sql.DatabaseMetaData.importedKeyNoAction, 3);
  harness.check(java.sql.DatabaseMetaData.importedKeySetDefault, 4);
  harness.check(java.sql.DatabaseMetaData.importedKeyInitiallyDeferred, 5);
  harness.check(java.sql.DatabaseMetaData.importedKeyInitiallyImmediate, 6);
  harness.check(java.sql.DatabaseMetaData.importedKeyNotDeferrable, 7);

  harness.check(java.sql.DatabaseMetaData.typeNoNulls, 0);
  harness.check(java.sql.DatabaseMetaData.typeNullable, 1);
  harness.check(java.sql.DatabaseMetaData.typeNullableUnknown, 2);
  harness.check(java.sql.DatabaseMetaData.typePredNone, 0);
  harness.check(java.sql.DatabaseMetaData.typePredChar, 1);
  harness.check(java.sql.DatabaseMetaData.typePredBasic, 2);
  harness.check(java.sql.DatabaseMetaData.typeSearchable, 3);

  harness.check(java.sql.DatabaseMetaData.tableIndexStatistic, 0);
  harness.check(java.sql.DatabaseMetaData.tableIndexClustered, 1);
  harness.check(java.sql.DatabaseMetaData.tableIndexHashed, 2);
  harness.check(java.sql.DatabaseMetaData.tableIndexOther, 3);
}

} // class TestJdbc

