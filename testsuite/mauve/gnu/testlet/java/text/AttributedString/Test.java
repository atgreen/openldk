/*************************************************************************
/* Test.java -- Test java.text.AttributedString
/*
/* Copyright (c) 1999 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
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

package gnu.testlet.java.text.AttributedString;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Set;
import java.text.*;

public class Test implements Testlet
{

public void 
test(TestHarness harness)
{
  AttributedString as = new AttributedString("I really think that " +
    "java.text is the most bogus Java package ever designed.");

  as.addAttribute(AttributedCharacterIterator.Attribute.READING, "never");
  as.addAttribute(AttributedCharacterIterator.Attribute.LANGUAGE,
                  "bogosity", 9, 23);

  AttributedCharacterIterator aci0 = as.getIterator(null, 28, 60);
  Set s0 = aci0.getAllAttributeKeys();
  harness.check(s0.size(), 1, "Attribute key count");

  AttributedCharacterIterator aci = as.getIterator(null, 20, 29);

  Set s = aci.getAllAttributeKeys();
  harness.check(s.size(), 2);
  Object[] o = s.toArray();
  if (o.length > 0)
    for (int i = 0; i < o.length; i++)
      {
        harness.debug("Attribute Key: " + o[i].toString());
      }

  aci.first();
  int rl = aci.getRunLimit();
  harness.check(rl, 23, "getRunLimit");

  aci.setIndex(rl);
  rl = aci.getRunLimit(AttributedCharacterIterator.Attribute.LANGUAGE);
  harness.check(rl, 29, "getRunLimit");
  aci.first();

  StringBuffer result = new StringBuffer("");
  do
    {
      result.append(aci.current() + "");
    }
  while(aci.next() != CharacterIterator.DONE);
  harness.check(result.toString(), "java.text", "iterator text");
}

} // class Test

