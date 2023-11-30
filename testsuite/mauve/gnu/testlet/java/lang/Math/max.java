// Tags: JDK1.0

/* Copyright (C) 1999 Cygnus Solutions

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

package gnu.testlet.java.lang.Math;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class max implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.check (Double.toString (Math.max (0.0, -0.0)), "0.0");
      harness.check (Double.toString (Math.max (-0.0, -0.0)), "-0.0");
      harness.check (Double.toString (Math.max (-0.0, 0.0)), "0.0");
      harness.check (Double.toString (Math.max (0.0, 0.0)), "0.0");
      harness.check (Double.toString (Math.max (1.0, 2.0)), "2.0");
      harness.check (Double.toString (Math.max (2.0, 1.0)), "2.0");
      harness.check (Double.toString (Math.max (-1.0, -2.0)), "-1.0");
      harness.check (Double.toString (Math.max (-2.0, 1.0)), "1.0");
      harness.check (Double.toString (Math.max (1.0, -2.0)), "1.0");
      harness.check (Double.toString (Math.max (2.0, Double.NaN)), "NaN");
      harness.check (Double.toString (Math.max (Double.NaN, 2.0)), "NaN");
      harness.check (Double.toString (Math.max (Double.NEGATIVE_INFINITY, 
			       Double.POSITIVE_INFINITY)), 
		     "Infinity");
      harness.check (Double.toString (Math.max (Double.POSITIVE_INFINITY, 
			       Double.POSITIVE_INFINITY)), 
		     "Infinity");
      harness.check (Double.toString (Math.max (Double.NEGATIVE_INFINITY, 0.0)),
		     "0.0");
      harness.check (Double.toString (Math.max (Double.POSITIVE_INFINITY, 0.0)),
		     "Infinity");
      harness.check (Double.toString (Math.max (Math.PI, 0.0)),
		     Double.toString(Math.PI));

      harness.check (Float.toString (Math.max (0.0f, -0.0f)), "0.0");
      harness.check (Float.toString (Math.max (-0.0f, -0.0f)), "-0.0");
      harness.check (Float.toString (Math.max (-0.0f, 0.0f)), "0.0");
      harness.check (Float.toString (Math.max (0.0f, 0.0f)), "0.0");
      harness.check (Float.toString (Math.max (1.0f, 2.0f)), "2.0");
      harness.check (Float.toString (Math.max (2.0f, 1.0f)), "2.0");
      harness.check (Float.toString (Math.max (-1.0f, -2.0f)), "-1.0");
      harness.check (Float.toString (Math.max (-2.0f, 1.0f)), "1.0");
      harness.check (Float.toString (Math.max (1.0f, -2.0f)), "1.0");
      harness.check (Float.toString (Math.max (2.0f, Float.NaN)), "NaN");
      harness.check (Float.toString (Math.max (Float.NaN, 2.0f)), "NaN");
      harness.check (Float.toString (Math.max (Float.NEGATIVE_INFINITY, 
			       Float.POSITIVE_INFINITY)), 
		     "Infinity");
      harness.check (Float.toString (Math.max (Float.POSITIVE_INFINITY, 
			       Float.POSITIVE_INFINITY)), 
		     "Infinity");
      harness.check (Float.toString (Math.max (Float.NEGATIVE_INFINITY, 0.0f)),
		     "0.0");
      harness.check (Float.toString (Math.max (Float.POSITIVE_INFINITY, 0.0f)),
		     "Infinity");
      harness.check (Float.toString (Math.max ((float)Math.PI, 0.0f)),
		     Float.toString((float)Math.PI));
    }
}

