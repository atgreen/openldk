// Tags: JDK1.5

/*
   Copyright (C) 2006 Michael Koch

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

package gnu.testlet.java.net.InetSocketAddress;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.net.*;

public class createUnresolved implements Testlet
{
  public void test (TestHarness harness)
  {
    boolean ok = false;

    try
    {
      InetSocketAddress.createUnresolved(null, 2000);
    }
    catch (IllegalArgumentException e)
    {
      ok = true;
    }

    harness.check(ok);

    ok = false;

    try
    {
      InetSocketAddress.createUnresolved("www.classpath.org", -1);
    }
    catch (IllegalArgumentException e)
    {
      ok = true;
    }

    harness.check(ok);

    ok = false;

    try
    {
      InetSocketAddress.createUnresolved("www.classpath.org", 65536);
    }
    catch (IllegalArgumentException e)
    {
      ok = true;
    }
    
    harness.check(ok);

    InetSocketAddress sa = InetSocketAddress.createUnresolved("www.classpath.org", 80);

    harness.check(sa.isUnresolved());
  }
}
