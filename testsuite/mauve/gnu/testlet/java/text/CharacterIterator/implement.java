/*************************************************************************
/* implement.java -- Test interface java.text.CharacterIterator
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

// Tags: JDK1.1

package gnu.testlet.java.text.CharacterIterator;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;

public class implement implements CharacterIterator, Testlet
{

public void 
test(TestHarness harness)
{
  harness.check(true, "Correctly implemented CharacterIterator");
}

public Object
clone()
{
  return(null);
}
public char
current()
{
  return('0');
}
public char
first()
{
  return('0');
}
public int
getBeginIndex()
{
  return(0);
}
public int
getEndIndex()
{
  return(0);
}
public int
getIndex()
{
  return(0);
}
public char
last()
{
  return('0');
}
public char
next()
{
  return('0');
}
public char
previous()
{
  return('0');
}
public char
setIndex(int pos)
{
  return('0');
}

} // class implement

