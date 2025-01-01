// Tags: JDK1.0

// Copyright (C) 2005 Red Hat, Inc.

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

package gnu.testlet.java.util.Observable;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.Observable;
import java.util.Observer;

public class observable implements Testlet
{
  public void test (TestHarness harness)
  {
    Observable o = new Observable();

    harness.checkPoint("adding null Observer");
    boolean ok = false;
    try
      {
	o.addObserver(null);
      }
    catch (NullPointerException _)
      {
	ok = true;
      }
    harness.check(ok);
  }
}
