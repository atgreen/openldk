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

public class min implements Testlet
{
  public void test (TestHarness harness)
    {
      harness.checkPoint("Small doubles");
      harness.check (Double.toString (Math.min (0.0, -0.0)), "-0.0");
      harness.check (Double.toString (Math.min (-0.0, -0.0)), "-0.0");
      harness.check (Double.toString (Math.min (-0.0, 0.0)), "-0.0");
      harness.check (Double.toString (Math.min (0.0, 0.0)), "0.0");
      harness.check (Double.toString (Math.min (1.0, 2.0)), "1.0");
      harness.check (Double.toString (Math.min (2.0, 1.0)), "1.0");
      harness.check (Double.toString (Math.min (-1.0, -2.0)), "-2.0");
      harness.check (Double.toString (Math.min (-2.0, 1.0)), "-2.0");
      harness.check (Double.toString (Math.min (1.0, -2.0)), "-2.0");

      harness.checkPoint("Double NaNs");

      harness.check (Double.isNaN(Double.NaN));
      harness.check (Double.isNaN( 0.0d/0.0d ));

      harness.checkPoint("Double NaN comparisons");

      harness.check (Double.toString (Math.min (2.0, Double.NaN)), "NaN");
      harness.check (Double.toString (Math.min (Double.NaN, 2.0)), "NaN");
      //      System.err.println(Double.toString (Math.min (Double.NaN, 2.0)));
      harness.check (Math.min (Double.NaN, 2.0), Double.NaN);

      harness.checkPoint("Double infinities");

      harness.check (Double.toString (Math.min (Double.NEGATIVE_INFINITY, 
			       Double.POSITIVE_INFINITY)), 
		     "-Infinity");
      harness.check (Math.min (Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY), Double.NEGATIVE_INFINITY);
      harness.check (Double.toString (Math.min (Double.POSITIVE_INFINITY, 
			       Double.POSITIVE_INFINITY)), 
		     "Infinity");
      harness.check (Double.toString (Math.min (Double.NEGATIVE_INFINITY, 0.0)),
		     "-Infinity");
      harness.check (Double.toString (Math.min (Double.POSITIVE_INFINITY, 0.0)),
		     "0.0");

      harness.checkPoint("Double pi");

      harness.check (Double.toString (Math.max (Math.PI, 0.0)),
		     Double.toString(Math.PI));

      harness.checkPoint("Small floats");

      harness.check (Float.toString (Math.min (0.0f, -0.0f)), "-0.0");
      harness.check (Float.toString (Math.min (-0.0f, -0.0f)), "-0.0");
      harness.check (Float.toString (Math.min (-0.0f, 0.0f)), "-0.0");
      harness.check (Float.toString (Math.min (0.0f, 0.0f)), "0.0");
      harness.check (Float.toString (Math.min (1.0f, 2.0f)), "1.0");
      harness.check (Float.toString (Math.min (2.0f, 1.0f)), "1.0");
      harness.check (Float.toString (Math.min (-1.0f, -2.0f)), "-2.0");
      harness.check (Math.min (-1.0f, -2.0f), -2.0);
      harness.check (Float.toString (Math.min (-2.0f, 1.0f)), "-2.0");
      harness.check (Float.toString (Math.min (1.0f, -2.0f)), "-2.0");

      harness.checkPoint("Float NaNs");

      harness.check (Float.toString (Math.min (2.0f, Float.NaN)), "NaN");
      harness.check (Math.min (2.0f, Float.NaN), Float.NaN);
      harness.check (Float.toString (Math.min (Float.NaN, 2.0f)), "NaN");
      harness.check (Math.min (Float.NaN, 2.0f), Float.NaN);

      harness.checkPoint("Float infinities");

      harness.check (Float.toString (Math.min (Float.NEGATIVE_INFINITY, 
			       Float.POSITIVE_INFINITY)), 
		     "-Infinity");
      harness.check (Math.min (Float.NEGATIVE_INFINITY, 
			       Float.POSITIVE_INFINITY),Float.NEGATIVE_INFINITY);
      harness.check (Float.toString (Math.min (Float.POSITIVE_INFINITY, 
			       Float.POSITIVE_INFINITY)), 
		     "Infinity");
      harness.check (Float.toString (Math.min (Float.NEGATIVE_INFINITY, 0.0f)),
		     "-Infinity");
      harness.check (Math.min (Float.NEGATIVE_INFINITY, 0.0f), Float.NEGATIVE_INFINITY);
      harness.check (Float.toString (Math.min (Float.POSITIVE_INFINITY, 0.0f)),
		     "0.0");

      harness.checkPoint("Float pi");

      harness.check (Float.toString (Math.max ((float)Math.PI, 0.0f)),
		     Float.toString((float)Math.PI));
    }
}


