// Tags: JDK1.4
// Uses: TestService

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

import java.util.List;
import java.util.LinkedList;

import javax.imageio.spi.RegisterableService;
import javax.imageio.spi.ServiceRegistry;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class registerServiceProvider
  implements Testlet
{
  public void test(TestHarness h)
  {
    Throwable caught;
    ServiceRegistry registry;
    SomeService s1 = new SomeService();
    TestService s2 = new TestService();

    List categories = new LinkedList();
    categories.add(SomeService.class);
    categories.add(RegisterableService.class);
    registry = new ServiceRegistry(categories.iterator());

    // Check #1: Null argument --> IllegalArgumentException.
    caught = null;
    try
      {
        registry.registerServiceProvider(null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException);

    // Check #2: Register a service that does not implement
    // RegisterableService.
    registry.registerServiceProvider(s1);
    h.check(registry.contains(s1));

    // Check #3: Register a RegisterableService.
    registry.registerServiceProvider(s2);
    h.check(registry.contains(s2));

    // Check #4.
    h.check(s2.numRegistrations, 1);

    // Check #5.
    h.check(s2.lastRegisteredRegistry, registry);

    // Check #6.
    h.check(s2.lastRegisteredCategory, RegisterableService.class);
  }


  private static class SomeService
  {
  }
}
