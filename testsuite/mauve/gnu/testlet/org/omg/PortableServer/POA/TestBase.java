// Copyright (c) IONA Technologies, Inc. Waltham, MA, USA.

// Adapted for Mauve by Audrius Meskauskas <audriusa@bluewin.ch>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

/*
This code originally came from the OMG's CORBA Open Source Testing project,
which lived at cost.omg.org. That site no longer exists.

All the contributing companies agreed to release their tests under the
terms of the GNU Lesser General Public License, available in the file
COPYING.LIB.

The code has been modified integrating into Mauve test environment and
removing tests that are not yet supported by Suns jre 1.4. Hence the license
is now GPL.

We downloaded the code from http://sourceforge.net/projects/corba-cost/,
administrated by Duncan Grisby.
*/

// **********************************************************************
//
// Copyright (c) 2001
// IONA Technologies, Inc.
// Waltham, MA, USA
//
// All Rights Reserved
//
// **********************************************************************


package gnu.testlet.org.omg.PortableServer.POA;

import gnu.testlet.TestHarness;

public class TestBase
{
  public TestHarness harness;

  public void TEST(boolean expr, String why)
  {
    harness.check(expr, why);
  }

  public void TEST(boolean expr)
  {
    harness.check(expr);
  }

  public void fail(Throwable ex)
  {
    harness.fail("Failed due " + ex + ":" + ex.getCause());
  }

  public org.omg.CORBA.TypeCode getOrigType(org.omg.CORBA.TypeCode tc)
  {
    org.omg.CORBA.TypeCode result = tc;

    try
      {
        while (result.kind() == org.omg.CORBA.TCKind.tk_alias)
          result = result.content_type();
      }
    catch (org.omg.CORBA.TypeCodePackage.BadKind ex)
      {
        harness.fail("Unexpected " + ex);
      }

    return result;
  }
}