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
public class getServiceProviderByClass
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
    registry.registerServiceProvider("foo", String.class);
    registry.registerServiceProvider("bar", String.class);

    // Check #1.
    caught = null;
    try
      {
        registry.getServiceProviderByClass(null);
      }
    catch (Exception ex)
      {
        caught = ex;
      }
    h.check(caught instanceof IllegalArgumentException);

    // Check #2.
    h.check(registry.getServiceProviderByClass(RegisterableService.class),
            null);

    // Check #3.
    Object sp = registry.getServiceProviderByClass(String.class);
    h.check(sp == "foo" || sp == "bar");
  }
}
