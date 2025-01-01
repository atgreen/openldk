/* TestAccessibleContext.java -- An AccessibleContext implementation for testing
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: not-a-test

package gnu.testlet.javax.accessibility.AccessibleContext;

import java.util.Locale;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.accessibility.AccessibleStateSet;

/**
 * A concrete subclass of AccessibleContext for testing.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class TestAccessibleContext extends AccessibleContext
{

  public AccessibleRole getAccessibleRole()
  {
    return null;
  }

  public AccessibleStateSet getAccessibleStateSet()
  {
    return null;
  }

  public int getAccessibleIndexInParent()
  {
    return 0;
  }

  public int getAccessibleChildrenCount()
  {
    return 0;
  }

  public Accessible getAccessibleChild(int i)
  {
    return null;
  }

  public Locale getLocale()
  {
    return null;
  }

}
