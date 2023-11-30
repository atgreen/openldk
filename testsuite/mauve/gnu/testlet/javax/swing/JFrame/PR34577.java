// Tags: JDK1.2

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

package gnu.testlet.javax.swing.JFrame;

import javax.swing.JFrame;
import javax.swing.JRootPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This checks for the bug found in PR34577, namely that
 * an exception is thrown when a subclass attempts to change
 * the root pane.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class PR34577
  implements Testlet
{

  public void test(TestHarness harness)
  {
    new TestFrame(harness);
  }

  static class TestFrame
    extends JFrame
  {

    public TestFrame(TestHarness h)
    {
      super("TestFrame");
      try
	{
	  setRootPane(new JRootPane());
	  h.check(true, "Root pane changed successfully.");
	}
      catch (IllegalArgumentException e)
	{
	  h.debug(e);
	  h.fail("Root pane could not be changed.");
	}
    }
  }

}
