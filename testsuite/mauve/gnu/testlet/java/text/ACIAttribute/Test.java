/*************************************************************************
/* Test.java -- Test java.text.AttributedCharacterIterator.Attribute
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

package gnu.testlet.java.text.ACIAttribute;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;

public class Test extends AttributedCharacterIterator.Attribute 
                          implements Testlet
{

// Stop bogus compile problems
public
Test()
{
  super("FUCKYOU");
}

public
Test(String name)
{
  super(name);
}

public void 
test(TestHarness harness)
{
  Test acia = new Test("HACKER");

  harness.check(acia.getName(), "HACKER", "getName()");
  harness.check(acia.toString(),
    "gnu.testlet.java.text.ACIAttribute.Test(HACKER)", "toString()");
  harness.check(acia.equals(acia), "equals() true");
  harness.check(!acia.equals(new Test("HACKER")), "equals() false");

  // This just makes sure the variables exist
  harness.debug(AttributedCharacterIterator.Attribute.LANGUAGE.toString());
  harness.debug(AttributedCharacterIterator.Attribute.READING.toString());
  harness.debug(AttributedCharacterIterator.Attribute.INPUT_METHOD_SEGMENT.toString());
}

} // class Test

