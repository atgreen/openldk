// Tags: JDK1.5

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

package gnu.testlet.javax.management.openmbean.ArrayType;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.HashMap;
import java.util.Map;

import javax.management.openmbean.ArrayType;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

/**
 * Tests {@link ArrayType#isValue(Object)}.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class IsValue
  implements Testlet
{
  
  public void test(TestHarness h)
  {
    ArrayType type = ArrayType.getPrimitiveArrayType(int[].class);
    h.check(!type.isValue(null), "Null value check");
    h.check(!type.isValue(3), "Non-array value check");
    h.check(type.isValue(new int[]{3}), "Primitive int array value check");
    h.check(!type.isValue(new Integer[]{3}), "Integer array value check");
    try
      {
	CompositeType ctype = new CompositeType("Test","Test",new String[]{"name"},
						new String[]{"Name"},
						new OpenType[] { SimpleType.STRING});
	Map<String,String> data = new HashMap<String,String>();
	data.put("name", "Bob");
	CompositeData cdata = new CompositeDataSupport(ctype, data);
	CompositeData[] cdataarr = new CompositeData[] { cdata };
	ArrayType type2 = new ArrayType(1, ctype);
	h.check(type2.isValue(cdataarr), "Composite data check");
	TabularType ttype = new TabularType("Test","Test",ctype,new String[]{"name"});
	TabularData tdata = new TabularDataSupport(ttype);
	tdata.put(cdata);
	TabularData[] tdataarr = new TabularData[] {tdata};
	ArrayType type3 = new ArrayType(1, ttype);
	h.check(type3.isValue(tdataarr), "Tabular data check");
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }
									   
  }
    
  
}
