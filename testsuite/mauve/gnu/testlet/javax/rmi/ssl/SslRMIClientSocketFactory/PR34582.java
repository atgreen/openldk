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

package gnu.testlet.javax.rmi.ssl.SslRMIClientSocketFactory;

import javax.rmi.ssl.SslRMIClientSocketFactory;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This checks for the bug found in PR34582, namely that
 * creating an instance of the class fails with a
 * {@code NullPointerException}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class PR34582
  implements Testlet
{

  public void test(TestHarness h)
  {
    try
      {
	new SslRMIClientSocketFactory();
	h.check(true, "Factory created succesfully.");
	}
      catch (Exception e)
	{
	  h.debug(e);
	  h.fail("Factory could not be created.");
	}
  }

}
