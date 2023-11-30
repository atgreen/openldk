// Tags: not-a-test

/* Copyright (C) 1999 Artur Biesiadowski

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

// Originally, this was implemented as an inner-class.  However, we
// have resolved, to the extent that is possible, to restrict
// ourselves to JLS 1.0 features.

package gnu.testlet.java.lang.Character;

public class CharInfo
{
	public String name;
	public String category;
	public int decimalDigit;
	public int digit;
	public int numericValue;
	public int uppercase;
	public int lowercase;
	public int titlecase;
	public int code;
}
