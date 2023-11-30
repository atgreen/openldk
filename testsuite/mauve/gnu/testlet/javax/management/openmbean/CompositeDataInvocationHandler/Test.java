// Tags: JDK1.5
// Uses: Person

// Copyright (C) 2007 Andrew John Hughes <gnu_andrew@member.fsf.org>

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

package gnu.testlet.javax.management.openmbean.CompositeDataInvocationHandler;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.reflect.Proxy;

import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataInvocationHandler;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * Tests {@link javax.management.openmbean.CompositeDataInvocationHandler}
 * for the {@link Person} interface.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class Test
  implements Testlet
{
  
  public void test(TestHarness h)
  {
    try
      {
	CompositeType type = new CompositeType("Person", "A person",
					       new String[] {"name","Birthday","alive"},
					       new String[] {"name","birthday","death status"},
					       new OpenType[] { SimpleType.STRING,
								SimpleType.DATE,
								SimpleType.BOOLEAN });
	Map map = new HashMap<String,Object>();
	Calendar cal = Calendar.getInstance();
	cal.set(1982, 11, 31);
	map.put("name","Andrew");
	map.put("Birthday",cal.getTime());
	map.put("alive",true);
	CompositeData data = new CompositeDataSupport(type, map);
	Person p = (Person) Proxy.newProxyInstance(Person.class.getClassLoader(),
						   new Class[] { Person.class },
						   new CompositeDataInvocationHandler(data));
	h.checkPoint("Accessor tests");
	h.check(p.getName(), "Andrew");
	h.check(p.getBirthday(), cal.getTime());
	h.check(p.isAlive(), true);
	h.check(p.equals(p), "Reflection test");
	h.check(p.equals(p), "Consistency test");
	Person p2 = (Person) Proxy.newProxyInstance(Person.class.getClassLoader(),
						   new Class[] { Person.class },
						   new CompositeDataInvocationHandler(data));
	h.check(p.equals(p2), "Equality over creation test");
	h.check(p2.equals(p), "Symmetric test");
	Person p3 = (Person) Proxy.newProxyInstance(Person.class.getClassLoader(),
						   new Class[] { Person.class },
						   new CompositeDataInvocationHandler(data));
	h.check(p2.equals(p2), "Second equality over creation test");
	h.check(p.equals(p3), "Transitivity test");
	h.check(!p.equals(null), "Null test");
	h.check(p.hashCode(), p.hashCode(), "Hashcode reflection test");
	h.check(p.hashCode(), p.hashCode(), "Hashcode consistency test");
	h.check(p.hashCode(), p2.hashCode(), "Hashcode equality over creation test");
	h.check(p2.hashCode(), p3.hashCode(), "Hashcode second equality over creation test");
	h.check(p.hashCode(), p3.hashCode(), "Hashcode transitivity test");
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }

  }

}

