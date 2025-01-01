// Copyright (C) 2007 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>
// Including some tests by Bryce McKinlay.

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

// Tags: JDK1.1

package gnu.testlet.java.util.Calendar;

import java.util.Calendar;
import java.util.TimeZone;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class dstOffset implements Testlet
{
  public void test(TestHarness harness)
  {
    TimeZone t = TimeZone.getTimeZone("America/Toronto");
    Calendar c = Calendar.getInstance(t);
    harness.check(c.isSet(Calendar.DST_OFFSET));

    // Bryce's tests
    // http://article.gmane.org/gmane.comp.java.classpath.devel/4509

    c.set(2004, Calendar.NOVEMBER, 1);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 0);

    c.set(Calendar.DST_OFFSET, -10000);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == -10000);

    c.set(2004, Calendar.OCTOBER, 1);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 3600000);

    c.set(Calendar.DST_OFFSET, -10000);    
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == -10000);

    // Gary's tests
    // Tests marked with XXX are not strictly necessary; calling
    // get() on unset field shouldn't really be allowed.

    c.clear();
    harness.check(!c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 0); // XXX

    // DST 

    c.setTimeInMillis(1175595146188L);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 3600000);

    c.clear(Calendar.DST_OFFSET);
    harness.check(!c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 3600000); // XXX

    c.set(Calendar.DST_OFFSET, 1800000);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 1800000);

    c.setTimeInMillis(1175595146188L);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 3600000);

    // non-DST

    c.setTimeInMillis(1172916746188L);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 0);

    c.clear(Calendar.DST_OFFSET);
    harness.check(!c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 0); // XXX

    c.set(Calendar.DST_OFFSET, 1800000);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 1800000);

    c.setTimeInMillis(1172916746188L);
    harness.check(c.isSet(Calendar.DST_OFFSET));
    harness.check(c.get(Calendar.DST_OFFSET) == 0);
  }
}
