/*************************************************************************
/* TimestampTest.java - Test java.sql.Timestamp
/*
/* Copyright (c) 2003 Dalibor Topic (robilad@yahoo.de)
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

// Tags: JDK1.2

package gnu.testlet.java.sql.Timestamp;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.sql.*;

import java.util.SimpleTimeZone;
import java.util.TimeZone;

public class TimestampTest implements Testlet
{
  public void
  test(TestHarness harness)
  {
     // Set a common timezone to get the same result everywhere.
    SimpleTimeZone stz = new SimpleTimeZone(-5 * 1000 * 3600, "GMT");
    TimeZone.setDefault(stz);

    try {
      Timestamp.valueOf("NoSuchTime");
      harness.check(false, "valueOf");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "valueOf");
    }


    Timestamp ts = new Timestamp(1099999999333L);
    harness.check(ts.getNanos() == 333000000, "getNanos");
    harness.check(ts.toString().equals("2004-11-09 06:33:19.333"),
                  "toString");
    harness.debug(ts.toString());

    ts.setNanos(42);
    harness.check(ts.getNanos() == 42, "getNanos");
    harness.check(ts.toString().equals("2004-11-09 06:33:19.000000042"),
                  "toString");

    harness.debug(ts.toString());

    ts.setNanos(0);
    harness.check(ts.getNanos() == 0, "getNanos");
    harness.check(ts.toString().equals("2004-11-09 06:33:19.0"),
                  "toString");

    harness.debug(ts.toString());

    Timestamp ts2 = new Timestamp(1099999999999L);
    harness.check(ts.equals(ts2) == false, "equals");
    ts.setNanos(999000000);
    harness.check(ts.equals(ts2), "equals");

    harness.debug(ts.toString());
    
    // Restore Timezone
    TimeZone.setDefault(null);
  }
}
