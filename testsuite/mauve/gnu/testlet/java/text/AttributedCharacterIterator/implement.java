/*************************************************************************
/* implement.java -- Test interface java.text.AttributedCharacterIterator
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
// Uses: CharItImpl

package gnu.testlet.java.text.AttributedCharacterIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;
import java.util.Set;
import java.util.Map;

public class implement extends CharItImpl
             implements AttributedCharacterIterator, Testlet
{

public void 
test(TestHarness harness)
{
  harness.check(true, "Correctly implemented AttributedCharacterIterator");
}

public int
getRunStart()
{
  return(0);
}
public int
getRunStart(AttributedCharacterIterator.Attribute attr)
{
  return(0);
}
public int
getRunStart(Set attrs)
{
  return(0);
}
public int
getRunLimit()
{
  return(0);
}
public int
getRunLimit(AttributedCharacterIterator.Attribute attr)
{
  return(0);
}
public int
getRunLimit(Set attrs)
{
  return(0);
}
public Map
getAttributes()
{
  return(null);
}
public Set
getAllAttributeKeys()
{
  return(null);
}
public Object
getAttribute(AttributedCharacterIterator.Attribute attr)
{
  return(null);
}

} // class implement

