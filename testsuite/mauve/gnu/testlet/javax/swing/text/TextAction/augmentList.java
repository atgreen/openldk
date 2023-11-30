// Tags: JDK1.4

// Copyright (C) 2004 Michael Koch <konqueror@gmx.de>
// Copyright (C) 2006 Mark Wielaard <mark@klomp.org>

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

package gnu.testlet.javax.swing.text.TextAction;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.event.ActionEvent;
import javax.swing.Action;
import javax.swing.text.TextAction;

public class augmentList
  implements Testlet
{
  private class TestAction extends TextAction
  {
    public TestAction(String name)
    {
      super(name);
    }

    public void actionPerformed(ActionEvent event)
    {
    }
  }
  
  public void test(TestHarness h)
  {
    boolean ok;
    Action[] result = null;
    TextAction[] empty = new TextAction[0];
    TextAction[] nullArray = new TextAction[2];
    nullArray[0] = null;
    nullArray[1] = null;
    TextAction[] data = new TextAction[2];
    data[0] = new TestAction("test 1");
    data[1] = new TestAction("test 2");
    
    ok = false;
    
    try
      {
	result = TextAction.augmentList(null, null);
      }
    catch (NullPointerException e)
      {
	ok = true;
      }
    catch (Exception e)
      {
      }

    h.check(ok, "invalid arguments");

    ok = false;
    
    try
      {
	result = TextAction.augmentList(empty, null);
      }
    catch (NullPointerException e)
      {
	ok = true;
      }
    catch (Exception e)
      {
      }

    h.check(ok, "invalid arguments");

    ok = false;
    
    try
      {
	result = TextAction.augmentList(null, empty);
      }
    catch (NullPointerException e)
      {
	ok = true;
      }
    catch (Exception e)
      {
      }

    h.check(ok, "invalid arguments");

    ok = false;
    
    try
      {
	result = TextAction.augmentList(empty, empty);
	ok = true;
      }
    catch (Exception e)
      {
      }

    h.check(ok, "invalid arguments");
    h.check(result.length, 0, "invalid array length");

    ok = false;
    
    try
      {
	result = TextAction.augmentList(data, data);
	ok = true;
      }
    catch (Exception e)
      {
      }

    // The length is guaranteed, and so is the content, but not the order.
    h.check(ok, "arguments");
    h.check(result.length, 2, "array length");
    
    h.check(result[0] == data[0] || result[0] == data[1], "content");
    h.check(result[1] == data[0] || result[1] == data[1], "content");
    h.check(result[0] != result[1], "content");
  }
}
