// Copyright (C) 2007 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>

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

// Tags: JDK1.5

package gnu.testlet.javax.management.ObjectName;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class apply implements Testlet
{
  private String[] domains = new String[] {
    "mauve", "m?uve", "*ve", "*au*", "m?*e", "mauv?", "m*v*", "*"};

  private boolean domainMatches(String a, String b)
  {
    // XXX This is very hacky.  The spec says that nothing matches
    // a pattern.  And all the patterns here match the non-pattern.
    return !b.contains("?") && !b.contains("*");
  }

  private String[] properties = new String[] {
    "foo=bar", "foo=bar,spam=eggs", "spam=eggs,foo=bar", "foo=bar,*", "*"};

  private boolean propertyMatches(String a, String b)
  {
    // Again, nothing matches a pattern.
    if (b.contains("*"))
      return false;
    // All the patterns here match the non-patterns.
    if (a.contains("*"))
      return true;
    // If they're the same length then they match (XXX hacky)
    return a.length() == b.length();
  }

  public void test(TestHarness harness)
  {
    for (int ida = 0; ida < domains.length; ida++)
      {
	for (int idb = 0; idb < domains.length; idb++)
	  {
	    for (int ipa = 0; ipa < properties.length; ipa++)
	      {
		for (int ipb = 0; ipb < properties.length; ipb++)
		  {
		    String da = domains[ida];
		    String db = domains[idb];
		    boolean dm = domainMatches(da, db);

		    String pa = properties[ipa];
		    String pb = properties[ipb];
		    boolean pm = propertyMatches(pa, pb);

		    String sa = da + ":" + pa;
		    String sb = db + ":" + pb;
		    boolean expect = dm && pm;

		    try
		      {
			ObjectName ona = new ObjectName(sa);
			ObjectName onb = new ObjectName(sb);
		    
			harness.check(ona.apply(onb) == expect,
				      sa + " should" +
				      (expect ? "" : " not") +
				      " match " + sb);
		      }
		    catch (MalformedObjectNameException e)
		      {
			harness.check(false);
			harness.debug(e);
		      }
		  }
	      }
	  }
      }
  }
}
