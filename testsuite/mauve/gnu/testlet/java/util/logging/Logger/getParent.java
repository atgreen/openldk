// Tags: JDK1.4
// Uses: TestLogger TestSecurityManager

// Copyright (C) 2006 Mark J. Wielaard  (mark@klomp.org)

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

package gnu.testlet.java.util.logging.Logger;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.Logger;


public class getParent implements Testlet
{
  public void test(TestHarness h)
  {
    Logger abcde = Logger.getLogger("a.b.c.d.e");
    Logger abc = Logger.getLogger("a.b.c");
    Logger ab = Logger.getLogger("a.b");
    Logger a = Logger.getLogger("a");
    Logger ax = Logger.getLogger("a.x");
    Logger axy = Logger.getLogger("a.x.y");
    Logger axyzw = Logger.getLogger("a.x.y.z.w");
    
    h.check(abcde.getParent(), abc);
    h.check(abc.getParent(), ab);
    h.check(ab.getParent(), a);
    h.check(ax.getParent(), a);
    h.check(axy.getParent(), ax); 
    h.check(axyzw.getParent(), axy); 

    Logger root = Logger.getLogger("");
    Logger anon = Logger.getAnonymousLogger();

    h.check(a.getParent(), root);
    h.check(anon.getParent(), root);
  }
}
