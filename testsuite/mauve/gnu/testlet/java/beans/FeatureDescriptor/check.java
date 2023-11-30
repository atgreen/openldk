// Written by Robert Schuster <thebohemian@gmx.net>

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

// Tags: JDK1.2

package gnu.testlet.java.beans.FeatureDescriptor;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import java.beans.FeatureDescriptor;

/**
 * <p>Basic tests for the <code>EventHandler</code></p>
 */
public class check implements Testlet
{

  public void test (TestHarness harness)
  {
	  FeatureDescriptor fd = new FeatureDescriptor();

	  harness.checkPoint("initialization");
	  
	  // At first everything should be null.
	  harness.check(fd.getName(), null);
	  harness.check(fd.getDisplayName(), null);
	  harness.check(fd.getShortDescription(), null);
	  
	  harness.checkPoint("name set");
	  fd.setName("foo");
	  // When only the name is set this will be forwarded to
	  // displayName and shortDescription.
	  
	  harness.check(fd.getName(), "foo");
	  harness.check(fd.getDisplayName(), "foo");
	  harness.check(fd.getShortDescription(), "foo");

	  harness.checkPoint("display name set");
	  fd.setDisplayName("baz");
	  // When displayName is set this will be forwarded to
	  // the unset shortDescription.
	  
	  harness.check(fd.getName(), "foo");
	  harness.check(fd.getDisplayName(), "baz");
	  harness.check(fd.getShortDescription(), "baz");
	  
	  harness.checkPoint("short description set");
	  fd.setShortDescription("bar");
	  // Finally everything has it's own value. 
	  
	  harness.check(fd.getName(), "foo");
	  harness.check(fd.getDisplayName(), "baz");
	  harness.check(fd.getShortDescription(), "bar");
  }
}
