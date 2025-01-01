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
 * Checks #3, #4, #7, and #9 are known to fail on JDK 1.4.1_01.
 * The author believes that these checks should succeed, so that
 * this shows a bug in the reference implementation of the JDK.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class setOrdering
  implements Testlet
{
  public void test(TestHarness h)
  {
    Throwable caught;
    ServiceRegistry registry;
    List categories;

    // Set-up.
    categories = new LinkedList();
    categories.add(String.class);
    categories.add(RegisterableService.class);
    registry = new ServiceRegistry(categories.iterator());
    registry.registerServiceProvider("sheep", String.class);
    registry.registerServiceProvider("goat", String.class);
    registry.registerServiceProvider("cow", String.class);

    // Check #1: Unknown category --> IllegalArgumentException.
    caught = null;
    try
      {
        registry.setOrdering(String.class, "foo",  null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException);

    // Check #2: Twice the same object --> IllegalArgumentException.
    caught = null;
    try
      {
        registry.setOrdering(String.class, "sheep", "sheep");
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException);

    // Check #3. Fails on JDK 1.4.1_01.
    h.check(registry.setOrdering(String.class, "cow", "sheep"));

    // Check #4. Fails on JDK 1.4.1_01.
    h.check(registry.setOrdering(String.class, "sheep", "goat"));

    // Check #5.
    h.check(!registry.setOrdering(String.class, "sheep", "goat"));

    // Check #6.
    h.check(!registry.setOrdering(String.class, "cow", "sheep"));

    // Check #7. Fails on JDK 1.4.1_01.
    h.check(registry.unsetOrdering(String.class, "cow", "sheep"));

    // Check #8.
    h.check(!registry.unsetOrdering(String.class, "cow", "sheep"));

    // Check #9. Fails on JDK 1.4.1_01.
    h.check(registry.setOrdering(String.class, "cow", "sheep"));
  }
}
