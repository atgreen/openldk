/* Copyright (C) 1999 Hewlett-Packard Company

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
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.0

package gnu.testlet.java.lang.Math;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class MathTest implements Testlet
{

  protected static TestHarness harness;
	public void test_Basics()
	{
	 	harness.check(!( Math.E != 2.7182818284590452354 ), 
			"test_Basics - 1");

		harness.check(!( Math.PI != 3.14159265358979323846 ), 
			"test_Basics - 2");
	}

	public void test_sincostan()
	{
		harness.check(!(  !(new Double(Math.sin( Double.NaN ))).isNaN() ), 
			"test_sincostan - 1");
		harness.check(!(  !(new Double(Math.sin( Double.POSITIVE_INFINITY ))).isNaN() ), 
			"test_sincostan - 2");
		harness.check(!(  !(new Double(Math.sin( Double.NEGATIVE_INFINITY ))).isNaN() ), 
			"test_sincostan - 3");
		harness.check(!(  Math.sin( -0.0 ) != -0.0 ), 
			"test_sincostan - 4");
		harness.check(!(  Math.sin( 0.0 ) != 0.0 ), 
			"test_sincostan - 5");

		harness.check(!(  !(new Double(Math.cos( Double.NaN ))).isNaN() ), 
			"test_sincostan - 6");
		harness.check(!(  !(new Double(Math.cos( Double.POSITIVE_INFINITY ))).isNaN() ), 
			"test_sincostan - 7");
		harness.check(!(  !(new Double(Math.cos( Double.NEGATIVE_INFINITY ))).isNaN() ), 
			"test_sincostan - 8");

		harness.check(!(  !(new Double(Math.tan( Double.NaN ))).isNaN() ), 
			"test_sincostan - 9");
		harness.check(!(  !(new Double(Math.tan( Double.POSITIVE_INFINITY ))).isNaN() ), 
			"test_sincostan - 10");
		harness.check(!(  !(new Double(Math.tan( Double.NEGATIVE_INFINITY ))).isNaN()), 
			"test_sincostan - 11");
		harness.check(!(  Math.tan( -0.0 ) != -0.0 ), 
			"test_sincostan - 12");
		harness.check(!(  Math.tan( 0.0 ) != 0.0 ), 
			"test_sincostan - 13");

		harness.check(!( Math.sin( Math.PI / 2.0 + Math.PI /6.0 ) <= 0.0 ), 
			"test_sincostan - 14");
		harness.check(!( Math.cos( Math.PI / 2.0 + Math.PI /6.0 ) >= 0.0 ), 
			"test_sincostan - 14");
		harness.check(!( Math.tan( Math.PI / 2.0 + Math.PI /6.0 ) >= 0.0 ), 
			"test_sincostan - 14");

	}

	public void test_asinacosatan()
	{
		harness.check(!(  !(new Double(Math.asin( Double.NaN ))).isNaN() ), 
			"test_asinacosatan - 1");
		harness.check(!(  Math.asin( -0.0 ) != -0.0 ), 
			"test_asinacosatan - 2");
		harness.check(!(  Math.asin( 0.0 ) != 0.0 ), 
			"test_asinacosatan - 3");

		harness.check(!(  !(new Double(Math.asin( 10.0 ))).isNaN() ), 
			"test_asinacosatan - 4");


		harness.check(!(  !(new Double(Math.acos( Double.NaN ))).isNaN() ), 
			"test_asinacosatan - 5");
		harness.check(!(  !(new Double(Math.acos( 10.0 ))).isNaN() ), 
			"test_asinacosatan - 6");


		harness.check(!(  !(new Double(Math.atan( Double.NaN ))).isNaN() ), 
			"test_asinacosatan - 7");
		harness.check(!(  Math.atan( -0.0 ) != -0.0 ), 
			"test_asinacosatan - 8");
		harness.check(!(  Math.atan( 0.0 ) != 0.0 ), 
			"test_asinacosatan - 9");

	}

	public void test_atan2()
	{
		harness.check(!(!(new Double( Math.atan2 (1.0 , Double.NaN ))).isNaN()), 
			"test_atan2 - 1");
		harness.check(!(!(new Double( Math.atan2 (Double.NaN,1.0 ))).isNaN()), 
			"test_atan2 - 2");

		harness.check(!(( Math.atan2(0.0, 10.0 ) != -0.0 ) ||
			( Math.atan2(2.0 , Double.POSITIVE_INFINITY ) != -0.0 )), 
			"test_atan2 - 3");

		harness.check(!(( Math.atan2(-0.0, 10.0 ) != -0.0 ) ||
			( Math.atan2(-2.0 , Double.POSITIVE_INFINITY ) != -0.0 )), 
			"test_atan2 - 4");

		harness.check(!(( Math.atan2(0.0, -10.0 ) != Math.PI) ||
			( Math.atan2(2.0 , Double.NEGATIVE_INFINITY ) != Math.PI )), 
			"test_atan2 - 4");

		harness.check(!(( Math.atan2(-0.0, -10.0 ) != -Math.PI) ||
			( Math.atan2(-2.0 , Double.NEGATIVE_INFINITY ) != -Math.PI )), 
			"test_atan2 - 5");

		harness.check(!(( Math.atan2(10.0, 0.0 ) != Math.PI/2.0) ||
			( Math.atan2(Double.POSITIVE_INFINITY , 3.0) != Math.PI /2.0)), 
			"test_atan2 - 6");

		harness.check(!(( Math.atan2(-10.0, 0.0 ) != -Math.PI/2.0) ||
			( Math.atan2(Double.NEGATIVE_INFINITY , 3.0) != -Math.PI /2.0)), 
			"test_atan2 - 7");

		harness.check(!(( Math.atan2(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY ) != Math.PI/4.0)), 
			"test_atan2 - 8");

		harness.check(!(( Math.atan2(Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY ) != Math.PI*3.0/4.0)), 
			"test_atan2 - 9");

		harness.check(!(( Math.atan2(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY ) != -Math.PI/4.0)), 
			"test_atan2 - 10");

		harness.check(!(( Math.atan2(Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY ) != -Math.PI*3.0/4.0)), 
			"test_atan2 - 11");
	}

	public void test_exp()
	{
		harness.check(!( !(new Double(Math.exp( Double.NaN ))).isNaN() ), 
			"test_exp - 1");

		harness.check(!( !(new Double(Math.exp( Double.POSITIVE_INFINITY))).isInfinite() ), 
			"test_exp - 2");

		harness.check(!( Math.exp( Double.NEGATIVE_INFINITY) != 0.0 ), 
			"test_exp - 3");
	}

	public void test_log()
	{														
		harness.check(!( !(new Double(Math.log( Double.NaN ))).isNaN() ), 
			"test_log - 1");
		harness.check(!( !(new Double(Math.log( -1.0 ))).isNaN() ), 
			"test_log - 2");

		harness.check(!( !(new Double(Math.log( Double.POSITIVE_INFINITY ))).isInfinite() ), 
			"test_log - 3");
	}

	public void test_sqrt()
	{
		harness.check(!( !(new Double(Math.sqrt( Double.NaN ))).isNaN() ||
			 !(new Double(Math.sqrt( -10.0 ))).isNaN()), 
			"test_sqrt - 1");

		harness.check(!( !(new Double(Math.sqrt( Double.NaN ))).isNaN() ||
			 !(new Double(Math.sqrt( -10.0 ))).isNaN()), 
			"test_sqrt - 2");

		harness.check(!( !(new Double(Math.sqrt( Double.POSITIVE_INFINITY))).isInfinite()), 
			"test_sqrt - 3");

		harness.check(!( Math.sqrt( -0.0) != -0.0 ||  Math.sqrt( 0.0) != 0.0 ), 
			"test_sqrt - 4");

		harness.check(!( Math.sqrt( -0.0) != -0.0 ||  Math.sqrt( 0.0) != 0.0 ), 
			"test_sqrt - 5");


		double sq = Math.sqrt(4.0);
		harness.check(!(!( sq >= 1.9999 &&  sq <= 2.111 )), 
			"test_sqrt - 6");
	}

	public void test_pow()
	{
		harness.check(!( Math.pow(1.0 , 0.0 ) != 1.0 ), 
			"test_pow - 1");

		harness.check(!( Math.pow(2.0 , -0.0 ) != 1.0 ), 
			"test_pow - 2");
		
		harness.check(!( Math.pow(123.0 , 1.0 ) != 123.0 ), 
			"test_pow - 3");

		harness.check(!( !(new Double(Math.pow( 10.0, Double.NaN ))).isNaN()), 
			"test_pow - 4");

		harness.check(!( !(new Double(Math.pow( Double.NaN, 1.0 ))).isNaN()), 
			"test_pow - 5");

		harness.check(!( !(new Double(Math.pow( 2.0, Double.POSITIVE_INFINITY ))).isInfinite()), 
			"test_pow - 6");

		harness.check(!( !(new Double(Math.pow( 0.5, Double.NEGATIVE_INFINITY ))).isInfinite()), 
			"test_pow - 7");

		harness.check(!( Math.pow( 1.5, Double.NEGATIVE_INFINITY ) != 0.0 ||
			 Math.pow( 0.5, Double.POSITIVE_INFINITY ) != 0.0), 
			"test_pow - 8");

		harness.check(!( !(new Double(Math.pow( 1.0, Double.POSITIVE_INFINITY ))).isNaN()), 
			"test_pow - 9");

		harness.check(!( Math.pow( 0.0, 1.0) != 0.0 ||
			 Math.pow( Double.POSITIVE_INFINITY , -1.0 ) != 0.0), 
			"test_pow - 10");

		harness.check(!( !(new Double(Math.pow( 0.0, -1.0 ))).isInfinite() ||
			 !(new Double(Math.pow( Double.POSITIVE_INFINITY, 1.0 ))).isInfinite() ), 
			"test_pow - 11");

		harness.check(!( Math.pow( -0.0, 5.0) != -0.0 ||
			 Math.pow( Double.NEGATIVE_INFINITY , -7.0 ) != -0.0), 
			"test_pow - 12");

		harness.check(!( Math.pow( -2.0, 6.0) != Math.pow(2.0,6.0)), 
			"test_pow - 13");

		harness.check(!( Math.pow( -2.0, 5.0) != -Math.pow(2.0,5.0)), 
			"test_pow - 14");

	}

	public void test_IEEEremainder()
	{
		harness.check(!( !(new Double(Math.IEEEremainder( Double.NaN, 1.0 ))).isNaN()), 
			"test_IEEEremainder - 1");
		harness.check(!( !(new Double(Math.IEEEremainder( 1.0,Double.NaN))).isNaN()),  
			"test_IEEEremainder - 2");
		harness.check(!( !(new Double(Math.IEEEremainder( Double.POSITIVE_INFINITY , 2.0))).isNaN()), 
			"test_IEEEremainder - 3");
		harness.check(!( !(new Double(Math.IEEEremainder( 2.0,0.0))).isNaN() ), 
			"test_IEEEremainder - 4");
		harness.check(!( Math.IEEEremainder( 3.0, Double.POSITIVE_INFINITY ) != 3.0 ), 
			"test_IEEEremainder - 5");
	}
	
	public void test_ceil()
	{
		harness.check(!( Math.ceil(5.0) != 5.0 ), 
			"test_ceil - 1");

		harness.check(!( Math.ceil(0.0) != 0.0 || Math.ceil(-0.0) != -0.0 ), 
			"test_ceil - 2");

		harness.check(!( !(new Double(Math.ceil(Double.POSITIVE_INFINITY))).isInfinite() ||
			 !(new Double(Math.ceil(Double.NaN))).isNaN()), 
			"test_ceil - 3");

		harness.check(!( Math.ceil(-0.5) != -0.0 ), 
			"test_ceil - 4");

		harness.check(!( Math.ceil( 2.5 ) != 3.0 ), 
			"test_ceil - 5");


	}

	public void test_floor()
	{
		harness.check(!( Math.floor(5.0) != 5.0 ), 
			"test_floor - 1");

		harness.check(!( Math.floor(2.5) != 2.0 ), 
			"test_floor - 2");

		harness.check(!( !(new Double(Math.floor(Double.POSITIVE_INFINITY))).isInfinite() ||
			 !(new Double(Math.floor(Double.NaN))).isNaN()), 
			"test_floor - 3");

		harness.check(!( Math.floor(0.0) != 0.0 || Math.floor(-0.0) != -0.0 ), 
			"test_floor - 4");

	}

	public void test_rint()
	{	
		harness.check(!( Math.rint( 2.3 ) != 2.0 ), 
			"test_rint - 1");

		harness.check(!( Math.rint( 2.7 ) != 3.0 ), 
			"test_rint - 2");


		harness.check(!(Math.rint( 2.5) != 2.0 ), 
			"test_rint - 3");

		harness.check(!( Math.rint( 2.0) != 2.0 ), 
			"test_rint - 4");

		harness.check(!( Math.rint( 2.0) != 2.0 ), 
			"test_rint - 5");

		harness.check(!( !(new Double(Math.rint(Double.POSITIVE_INFINITY))).isInfinite() ||
			 !(new Double(Math.rint(Double.NaN))).isNaN()), 
			"test_rint - 6");

		harness.check(!( Math.rint(0.0) != 0.0 || Math.rint(-0.0) != -0.0 ), 
			"test_rint - 7");
	}

	public void test_round()
	{
		harness.check(!( Math.round( 3.4 ) != 3 ), 
			"test_round - 1");

		harness.check(!( Math.round( 9.55 ) != 10 ), 
			"test_round - 2");

		harness.check(!( Math.round(Double.NaN) != 0 ), 
			"test_round - 3");

		float f1 = Integer.MIN_VALUE;
		f1 -= 5;
		harness.check(!( Math.round(f1) != Integer.MIN_VALUE ||
			 Math.round(Float.NEGATIVE_INFINITY) != Integer.MIN_VALUE ), 
			"test_round - 4");

		f1 = Integer.MAX_VALUE;
		f1 += 5;
		harness.check(!( Math.round(f1) != Integer.MAX_VALUE ||
			 Math.round(Float.POSITIVE_INFINITY) != Integer.MAX_VALUE ), 
			"test_round - 5");

		double d1 = Long.MIN_VALUE;
		d1 -= 5;
		harness.check(!( Math.round(d1) != Long.MIN_VALUE ||
			 Math.round(Double.NEGATIVE_INFINITY) != Long.MIN_VALUE ), 
			"test_round - 6");

		d1 = Long.MAX_VALUE;
		d1 += 5;
		harness.check(!( Math.round(d1) != Long.MAX_VALUE ||
			 Math.round(Double.POSITIVE_INFINITY) != Long.MAX_VALUE ), 
			"test_round - 7");


		harness.check(!( Math.round( 3.4f ) != 3 ), 
			"test_round - 8");

		harness.check(!( Math.round( 9.55f ) != 10 ), 
			"test_round - 9");

		harness.check(!( Math.round(Float.NaN) != 0 ), 
			"test_round - 10");
	}														  

	public void test_random()
	{
		harness.check(!( Math.random() < 0.0 || Math.random() > 1.0 ), 
			"test_random - 1");
	}

	public void test_abs()
	{
		harness.check(!( Math.abs( 10 ) != 10 ),  
			"test_abs - 1");

		harness.check(!( Math.abs( -23 ) != 23 ), 
			"test_abs - 2");

		harness.check(!( Math.abs( Integer.MIN_VALUE ) != Integer.MIN_VALUE ), 
			"test_abs - 3" );
		
		harness.check(!( Math.abs(-0) != 0 ), 
			"test_abs - 4" );


		harness.check(!( Math.abs( 1000L ) != 1000 ),  
			"test_abs - 5");

		harness.check(!( Math.abs( -2334242L ) != 2334242 ), 
			"test_abs - 6");

		harness.check(!( Math.abs( Long.MIN_VALUE ) != Long.MIN_VALUE ), 
			"test_abs - 7" );
		
		harness.check(!( Math.abs( 0.0f ) != 0.0f || Math.abs(-0.0f) != 0.0f ), 
			"test_abs - 8" );
		
		harness.check(!( !(new Float(Math.abs( Float.POSITIVE_INFINITY ))).isInfinite() ), 
			"test_abs - 9" );

		harness.check(!( !(new Float(Math.abs( Float.NaN ))).isNaN() ), 
			"test_abs - 10" );

		harness.check(!( Math.abs( 23.34f ) != 23.34f ), 
			"test_abs - 11" );

	
		harness.check(!( Math.abs( 0.0 ) != 0.0 || Math.abs(-0.0) != 0.0 ), 
			"test_abs - 12" );
		
		harness.check(!( !(new Double(Math.abs( Double.POSITIVE_INFINITY ))).isInfinite() ), 
			"test_abs - 13" );

		harness.check(!( !(new Double(Math.abs( Double.NaN ))).isNaN() ), 
			"test_abs - 14" );

		harness.check(!( Math.abs( 23.34 ) != 23.34 ), 
			"test_abs - 15" );

	}

	public void test_min()
	{
		harness.check(!( Math.min( 100 , 12 ) != 12 ),  
			"test_min - 1" );

		harness.check(!( Math.min( Integer.MIN_VALUE , Integer.MIN_VALUE + 1 ) != Integer.MIN_VALUE ), 
			"test_min - 2" );

		harness.check(!( Math.min( Integer.MAX_VALUE , Integer.MAX_VALUE -1 ) != Integer.MAX_VALUE -1 ), 
			"test_min - 3" );
			
		harness.check(!( Math.min( 10 , 10 ) != 10 ), 
			"test_min - 4" );

		harness.check(!( Math.min( 0 , -0 ) != -0 ), 
			"test_min - 5" );


		harness.check(!( Math.min( 100L , 12L ) != 12L ),  
			"test_min - 6" );

		harness.check(!( Math.min( Long.MIN_VALUE , Long.MIN_VALUE + 1 ) != Long.MIN_VALUE ), 
			"test_min - 7" );

		harness.check(!( Math.min( Long.MAX_VALUE , Long.MAX_VALUE -1 ) != Long.MAX_VALUE -1 ), 
			"test_min - 8" );
			
		harness.check(!( Math.min( 10L , 10L ) != 10L ), 
			"test_min - 9" );

		harness.check(!( Math.min( 0L , -0L ) != -0L ), 
			"test_min - 10" );

		
		harness.check(!( Math.min( 23.4f , 12.3f ) != 12.3f ),  
			"test_min - 11" );

		harness.check(!( !(new Float(Math.min( Float.NaN ,  1.0f ))).isNaN()  ), 
			"test_min - 12" );

		harness.check(!( Math.min( 10.0f , 10.0f ) != 10.0f ), 
			"test_min - 13" );

		harness.check(!( Math.min( 0.0f , -0.0f ) != -0.0f ), 
			"test_min - 14" );


		harness.check(!( Math.min( 23.4 , 12.3 ) != 12.3 ),  
			"test_min - 15" );

		harness.check(!( !(new Double(Math.min( Double.NaN ,  1.0 ))).isNaN()  ), 
			"test_min - 16" );

		harness.check(!( Math.min( 10.0 , 10.0 ) != 10.0 ), 
			"test_min - 17" );

		harness.check(!( Math.min( 0.0 , -0.0 ) != -0.0 ), 
			"test_min - 18" );

	}

	public void test_max()
	{
		harness.check(!( Math.max( 100 , 12 ) != 100 ),  
			"test_max - 1" );

		harness.check(!( Math.max( Integer.MAX_VALUE , Integer.MAX_VALUE - 1 ) != Integer.MAX_VALUE ), 
			"test_max - 2" );

		harness.check(!( Math.max( Integer.MIN_VALUE , Integer.MIN_VALUE + 1 ) != Integer.MIN_VALUE +1 ), 
			"test_max - 3" );
			
		harness.check(!( Math.max( 10 , 10 ) != 10 ), 
			"test_max - 4" );

		harness.check(!( Math.max( 0 , -0 ) != 0 ), 
			"test_max - 5" );


		harness.check(!( Math.max( 100L , 12L ) != 100L ),  
			"test_max - 6" );

		harness.check(!( Math.max( Long.MAX_VALUE , Long.MAX_VALUE - 1 ) != Long.MAX_VALUE ), 
			"test_max - 7" );

		harness.check(!( Math.max( Long.MIN_VALUE , Long.MIN_VALUE +1 ) != Long.MIN_VALUE + 1 ), 
			"test_max - 8" );
			
		harness.check(!( Math.max( 10L , 10L ) != 10L ), 
			"test_max - 9" );

		harness.check(!( Math.max( 0L , -0L ) != 0L ), 
			"test_max - 10" );

		
		harness.check(!( Math.max( 23.4f , 12.3f ) != 23.4f ),  
			"test_max - 11" );

		harness.check(!( !(new Float(Math.max( Float.NaN ,  1.0f ))).isNaN()  ), 
			"test_max - 12" );

		harness.check(!( Math.max( 10.0f , 10.0f ) != 10.0f ), 
			"test_max - 13" );

		harness.check(!( Math.max( 0.0f , -0.0f ) != 0.0f ), 
			"test_max - 14" );


		harness.check(!( Math.max( 23.4 , 12.3 ) != 23.4 ),  
			"test_max - 15" );

		harness.check(!( !(new Double(Math.max( Double.NaN ,  1.0 ))).isNaN()  ), 
			"test_max - 16" );

		harness.check(!( Math.max( 10.0 , 10.0 ) != 10.0 ), 
			"test_max - 17" );

		harness.check(!( Math.max( 0.0 , -0.0 ) != 0.0 ), 
			"test_max - 18" );
	}


	public void testall()
	{
		test_Basics();
		test_sincostan();
		test_asinacosatan();
		test_atan2();
		test_log();
		test_exp();
		test_sqrt();
		test_pow();
		test_IEEEremainder();
		test_ceil();
		test_floor();
		test_rint();
		test_round();
		test_random();
		test_abs();
		test_min();
		test_max();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
