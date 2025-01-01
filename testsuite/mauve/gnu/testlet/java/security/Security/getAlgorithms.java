// $Id: getAlgorithms.java,v 1.3 2003/04/07 15:42:11 crawley Exp $
//
// Copyright (C) 2003, Free Software Foundation, Inc.
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.
//
// Tags: JDK1.4

package gnu.testlet.java.security.Security;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.security.Provider;
import java.security.Security;
import java.util.Set;
import java.util.Iterator;

/**
 * Test of <code>getAlgorithms(String)</code> method in {@link Security}.
 *
 * @version $Revision: 1.3 $
 * @see Security#getAlgorithms(String)
 */
public class getAlgorithms extends Provider implements Testlet
{
  public getAlgorithms()
  {
    super("getAlgorithms", 1.0, "");

    put("Coffee.Foo", "whatever");

    put("Tea.Bar", "whatever");
    put("Tea.Bar ImplementedIn", "Vapourware");
    put("Tea.Bar MinCapacity", "100");

    put("Tea.Baz", "whatever");
    put("Tea.Baz ImplementedIn", "Vapourware");
    put("Tea.Baz MinCapacity", "100");
  }

  public void test (TestHarness harness)
  {
    harness.checkPoint ("getAlgorithms");

    String signature, key;
    Set set = null;

    Security.addProvider(this);

    signature = "getAlgorithms(\"foo\")";
    set = Security.getAlgorithms("foo");
    harness.check(set != null && set.size() == 0, signature);

    signature = "getAlgorithms(\"Coffee\")";
    set = Security.getAlgorithms("Coffee");
    key = "Foo";
    if (set != null && set.size() >= 1)
      harness.check(containsKey(set, key), signature+": "+key);
    else
      harness.check(false, signature + ": set.size() < 1");

    signature = "getAlgorithms(\"Tea\")";
    set = Security.getAlgorithms("Tea");
    if (set != null && set.size() >= 2)
      {
        key = "Bar";
        harness.check(containsKey(set, key), signature+": "+key);
        key = "Baz";
        harness.check(containsKey(set, key), signature+": "+key);
      }
    else
      harness.check(false, signature + ": set.size() < 2");
  }

  private static final boolean containsKey(Set set, String key) {
    boolean result = false;
    for (Iterator it = set.iterator(); it.hasNext(); )
      {
        result = key.trim().equalsIgnoreCase(String.valueOf(it.next()).trim());
        if (result)
          break;
      }
    return result;
  }
}
