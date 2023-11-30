// Tags: JDK1.4

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.util.logging.LogRecord;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class setParameters
  implements Testlet
{
  public void test(TestHarness th)
  {
    LogRecord rec = new LogRecord(Level.CONFIG, "msg");
    Object[] params = new Object[] { "foo", "bar", "baz" };

    // Check #1.
    th.check(rec.getParameters(), null);

    // Check #2.
    rec.setParameters(params);
    th.check(rec.getParameters() == params);

    // Check #3.
    rec.setParameters(null);
    th.check(rec.getParameters(), null);
  }
}
