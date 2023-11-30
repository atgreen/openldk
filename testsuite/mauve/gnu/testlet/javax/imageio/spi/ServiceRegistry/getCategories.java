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

package gnu.testlet.javax.imageio.spi.ServiceRegistry;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.*;

import javax.imageio.spi.RegisterableService;
import javax.imageio.spi.ServiceRegistry;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getCategories
  implements Testlet
{
  public void test(TestHarness h)
  {
    ServiceRegistry registry;
    List categories;
    Set cats;

    // Check #1.
    registry = new ServiceRegistry(Collections.EMPTY_LIST.iterator());
    h.check(!registry.getCategories().hasNext());

    // Check #2, #3 and #4.
    categories = new LinkedList();
    categories.add(String.class);
    categories.add(RegisterableService.class);
    registry = new ServiceRegistry(categories.iterator());
    cats = new HashSet();
    for (Iterator iter = registry.getCategories(); iter.hasNext();)
      cats.add(iter.next());
    
    h.check(cats.size(), 2); // Check #2.
    h.check(cats.contains(String.class)); // Check #3.
    h.check(cats.contains(RegisterableService.class)); // Check #4.
  }
}
