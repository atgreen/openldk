/*************************************************************************
/* offset -- Test get/setOffset features in CollationElementIterator.
/*
/* Copyright (c) 2004 Guilhem Lavaux <guilhem.lavaux@free.fr>
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/
 
// Tags: JDK1.2

package gnu.testlet.java.text.CollationElementIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.CollationElementIterator;
import java.text.RuleBasedCollator;
import java.text.ParseException;

public class offset implements Testlet
{
  public void test(TestHarness harness)
  {
    try 
      {
	RuleBasedCollator collator = new RuleBasedCollator("<a,A<AB,ab<ABC,abc");
	CollationElementIterator iter;
	
	iter = collator.getCollationElementIterator("abaaABC");
	iter.setOffset(1);
	harness.check(iter.getOffset(), 0);

	iter.setOffset(0);
	iter.next();
	harness.check(iter.getOffset(), 2); 
	iter.next();
	harness.check(iter.getOffset(), 3); 
	iter.next();
	harness.check(iter.getOffset(), 4); 
	iter.next();
	harness.check(iter.getOffset(), 7); 
      }
    catch (ParseException e)
      {
	harness.fail("Unexpected parse error exception");
	harness.debug(e);
      }

  }
}
