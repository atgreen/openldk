// Tags: JDK1.6

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

package gnu.testlet.java.awt.Desktop;

import java.awt.Desktop;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This checks for the bug found in PR34580, namely that
 * isDesktopSupported() is inaccessible.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class PR34580
  implements Testlet
{

  public void test(TestHarness h)
  {
    try
      {
	Method m = Desktop.class.getMethod("isDesktopSupported");
	m.invoke(null);
	h.check(true, "isDesktopSupported() accessed successfully.");
      }
    catch (IllegalAccessException e)
      {
	h.debug(e);
	h.fail("isDesktopSupported() could not be accessed.");
      }
    catch (NoSuchMethodException e)
      {
	h.debug(e);
	h.fail("isDesktopSupported() is not implemented.");
      }
    catch (InvocationTargetException e)
      {
	h.debug(e);
	h.fail("isDesktopSupported() threw an exception: " + e);
      }
  }

}
