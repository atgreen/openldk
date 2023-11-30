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

import javax.management.openmbean.ArrayType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;

/**
 * Tests {@link ArrayType#equals(Object)}.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class Equals
  implements Testlet
{
  
  public void test(TestHarness h)
  {
    ArrayType type = ArrayType.getPrimitiveArrayType(int[].class);
    h.check(type.equals(type), "Reflection test");
    h.check(type.equals(type), "Consistency test");
    ArrayType type2 = ArrayType.getPrimitiveArrayType(int[].class);
    h.check(type.equals(type2), "Equality over creation test");
    h.check(type2.equals(type), "Symmetric test");
    ArrayType type3 = null;
    try
      {
	type3 = new ArrayType(SimpleType.INTEGER, true);
      }
    catch (OpenDataException e)
      {
	h.debug(e);
      }
    h.check(type2.equals(type3), "Equality over different creation test");
    h.check(type.equals(type3), "Transitivity test");
    h.check(!type.equals(null), "Null test");
  }
    
  
}
