/* Copyright (C) 1999, 2002 Hewlett-Packard Company

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

package gnu.testlet.java.lang.Float;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class FloatTest implements Testlet
{

  protected static TestHarness harness;
	public void test_Basics()
	{
		Float nan1 = new Float(0.0f/0.0f);
		Float nan2 = new Float(Float.NaN);
		float min1 = 1.4e-45f;
		float min2 = Float.MIN_VALUE;
		float max1 = 3.4028235e+38f;
		float max2 = Float.MAX_VALUE;
		float ninf1 = -1.0f/0.0f;
		float ninf2 = Float.NEGATIVE_INFINITY;
		float pinf1 = 1.0f/0.0f;
		float pinf2 = Float.POSITIVE_INFINITY;

		harness.check(!( min1 != min2 || max1 != max2 || 
			ninf1 != ninf2 || pinf1 != pinf2 ||
			!nan2.equals(nan1) ), 
			"test_Basics - 1" );

		Float i1 = new Float(100.5f);

		harness.check(!( i1.floatValue() != 100.5f ), 
			"test_Basics - 2" );


		try {
		harness.check(!( (new Float("234.34f")).floatValue() != 234.34f ), 
			"test_Basics - 3" );
		}
		catch ( NumberFormatException e )
		{
			harness.fail("test_Basics - 3" );
		}

                if (!System.getProperty("os.name").equals("VxWorks")){
                  // bug EJWcr00687, has not been fixed yet.
                  // Test is disabled for smallvm 2.0.1 release.
                  try {
                    harness.check(!( (new Float("1.4e-45f")).floatValue() != 1.4e-45f ), 
                      "test_Basics - 4" );
                  }
                  catch ( NumberFormatException e )
                    {
                      harness.fail("test_Basics - 4" );
                    }
                }

		try {
		    new Float("babu");
			harness.fail("test_Basics - 5" );
		}
		catch ( NumberFormatException e )
		{
		}

		harness.check(!( (new Float(3.4)).floatValue() != 3.4f ), 
			"test_Basics - 6" );


		Float nan = new Float(Float.NaN );
		harness.check(!( !nan.isNaN()), 
			"test_Basics - 7" );
		
		harness.check(!( (new Float(10.0f)).isNaN()), 
			"test_Basics - 8" );

		harness.check(!( !Float.isNaN( Float.NaN )), 
			"test_Basics - 9" );

		harness.check(!( !(new Float(Float.POSITIVE_INFINITY)).isInfinite()), 
			"test_Basics - 10" );

		harness.check(!( !(new Float(Float.NEGATIVE_INFINITY)).isInfinite()), 
			"test_Basics - 11" );
		harness.check(!( !(Float.isInfinite( Float.POSITIVE_INFINITY))), 
			"test_Basics - 12" );
		harness.check(!( !(Float.isInfinite( Float.NEGATIVE_INFINITY))), 
			"test_Basics - 13" );
		harness.check(!( Float.isInfinite( 2.30f )), 
			"test_Basics - 14" );

	}

	public void test_toString()
	{
		harness.check(!( !( new Float(123.0f)).toString().equals("123.0")), 
			"test_toString - 1" );
		harness.check(!( !( new Float(-44.5343f)).toString().equals("-44.5343")), 
			"test_toString - 2" );

		harness.check(!( !Float.toString( 23.04f ).equals ("23.04" )), 
			"test_toString - 3" );

		harness.check(!( !Float.toString( Float.NaN ).equals ("NaN" )), 
			"test_toString - 4" );

		harness.check(!( !Float.toString( Float.POSITIVE_INFINITY ).equals ("Infinity" )), 
			"test_toString - 5" );
		harness.check(!( !Float.toString( Float.NEGATIVE_INFINITY ).equals ("-Infinity" )), 
			"test_toString - 6" );

		harness.check(!( !Float.toString( 0.0f ).equals ("0.0" )), 
			"test_toString - 7" );

		String str;

		str = Float.toString( -0.0f );
		harness.check(!( !str.equals ("-0.0" )), 
			"test_toString - 8" );

		str = Float.toString( -912125.45f);
		if ( !str.equals ("-912125.44" )) {
			harness.fail("test_toString - 9" );
			System.out.println("Bug EJWcr00027");
			System.out.println("expected '-912125.45', got '" +
			    str + "'");
		}

		// The following case fails for some Sun JDKs (e.g. 1.3.1
		// and 1.4.0) where toString(0.001) returns "0.0010".  This
		// is contrary to the JDK 1.4 javadoc.  This particular
		// case has been noted as a comment to Sun Java bug #4642835
		str = Float.toString( 0.001f );
		harness.check(!( !Float.toString( 0.001f ).equals ("0.001" )), 
			"test_toString - 10" );

		str = Float.toString(33333333.33f );
		if ( !(new Float( str)).equals(new Float(33333333.33f))) {
			harness.fail("test_toString - 11" );
			System.out.println("Bug EJWcr00029");
			int i = Float.floatToIntBits(new Float( str).floatValue());
			int j = Float.floatToIntBits(new Float(33333333.33f).floatValue());
			int k = Float.floatToIntBits(33333333.33f);
			System.out.println("ours = " + Integer.toHexString(i));
			System.out.println("javac's = " + Integer.toHexString(j));
			System.out.println("plain constant = " + Integer.toHexString(k));
		}
		str = Float.toString(-123232324253.32f );
		if ( !(new Float( str)).equals(new Float(-123232324253.32f))) {
			harness.fail("test_toString - 12" );
			System.out.println("Bug EJWcr00030");
			int i = Float.floatToIntBits(new Float( str).floatValue());
			int j = Float.floatToIntBits(new Float(-123232324253.32f).floatValue());
			int k = Float.floatToIntBits(-123232324253.32f);
			System.out.println("ours = " + Integer.toHexString(i));
			System.out.println("javac's = " + Integer.toHexString(j));
			System.out.println("plain constant = " + Integer.toHexString(k));
		}
		str = Float.toString(1.243E10f);
		harness.check(!( !(new Float( str)).equals(new Float(1.243E10f))), 
			"test_toString - 13" );
		str = Float.toString(-23.43E33f);
		harness.check(!( !(new Float( str)).equals(new Float(-23.43E33f))), 
			"test_toString - 14" );

		str = Float.toString(Float.MIN_VALUE);
		if(!str.equals("1.4E-45")) {
		    harness.fail("test_toString - 15" );
		    harness.debug("Expected : 1.4E-45");
		    harness.debug("Got: " + str);
		}
	}

	public void test_equals()
	{
		Float i1 = new Float(2334.34E4);
		Float i2 = new Float(-2334.34E4);

		harness.check(!( !i1.equals( new Float(2334.34E4))), 
			"test_equals - 1" );
		harness.check(!( !i2.equals( new Float(-2334.34E4))), 
			"test_equals - 2" );

		
		harness.check(!( i1.equals( i2 )), 
			"test_equals - 3" );

		harness.check(!( i1.equals(null)), 
			"test_equals - 4" );

		float n1 = Float.NaN;
		float n2 = Float.NaN;

		harness.check(!( n1 == n2 ), 
			"test_equals - 5" );

		Float flt1 = new Float( Float.NaN);
		Float flt2 = new Float( Float.NaN);
		harness.check(!( !flt1.equals(flt2)), 
			"test_equals - 6" );

		harness.check(!( 0.0f != -0.0f ), 
			"test_equals - 7" );

		Float pzero = new Float( 0.0f );
		Float nzero = new Float( -0.0f );

		harness.check(!( pzero.equals(nzero) ), 
			"test_equals - 8" );

	}


	public void test_hashCode( )
	{
		Float flt1 = new Float(3.4028235e+38f);

		harness.check(!( flt1.hashCode() != Float.floatToIntBits( 3.4028235e+38f )), 
			"test_hashCode - 1");

		Float flt2 = new Float( -2343323354f );
		harness.check(!( flt2.hashCode() != Float.floatToIntBits( -2343323354f )), 
			"test_hashCode - 2");
	}

	public void test_intValue( )
	{
		Float b1 = new Float(3.4e+32f);
		Float b2 = new Float(-23.45f);

		int i1 = b1.intValue();
		int i2 = b2.intValue();

		harness.check(!( i1 != (int) 3.4e+32f),  
			"test_intValue - 1" );

		harness.check(!( i2 != (int) -23.45f ),  
			"test_intValue - 2" );
	}

	public void test_longValue( )
	{
		Float b1 = new Float(3.4e+15f);
		Float b2 = new Float(-23.45f);

		float b3 = 3.4e+15f;
		long l3 = (long)b3;

		harness.check(!( b1.longValue() != l3),  
			"test_longValue - 1" );

		b3 = -23.45f;
		l3 = (long)b3;
		harness.check(!( b2.longValue() != l3),  
			"test_longValue - 2" );
	}

	public void test_floatValue( )
	{
		Float b1 = new Float(3276.34f);
		Float b2 = new Float(-3276.32);

		harness.check(!( b1.floatValue() != 3276.34f ),  
			"test_floatValue - 1" );

		harness.check(!( b2.floatValue() != -3276.32f ),  
			"test_floatValue - 2" );
	}

	public void test_doubleValue( )
	{
		Float b1 = new Float(0.0f);
		Float b2 = new Float(30.0f);

		harness.check(!( b1.doubleValue() != 0.0 ),  
			"test_doubleValue - 1" );

		harness.check(!( b2.doubleValue() != 30.0 ),  
			"test_doubleValue - 2" );
	}

	public void test_valueOf( )
	{
		try {
			Float.valueOf(null);
			harness.fail("test_valueOf - 1" );
		}
		catch ( NumberFormatException nfe )
		  {harness.check(false, "test_valueOf null should throw NullPointerException");}
		catch ( NullPointerException e )
		  {harness.check(true, "test_valueOf null");}


		try {
			Float.valueOf("Kona");
			harness.fail("test_valueOf - 2" );
		}catch( NumberFormatException e)
		  {harness.check(true, "test_valueOf Kona");}


		try {
			harness.check(!( Float.valueOf( "3.4e+32f" ).floatValue() != 3.4e+32f ),  
				"test_valueOf - 3" );
		}catch( NumberFormatException e)
		  {harness.check(false, "test_valueOf 3.4e+32f");}

		try {
			harness.check(!( Float.valueOf(" -23.45f    ").floatValue() != -23.45f ),  
				"test_valueOf - 4" );
		}catch( NumberFormatException e)
		  {harness.check(false, "test_valueOf \" -23.45f    \"");}

	}


	public void test_parseFloat( )
	{
		try {
			Float.parseFloat(null);
			harness.fail("test_parseFloat - 1" );
		}
		catch ( NumberFormatException nfe )
		  {harness.check(false, "test_parseFloat null should throw NullPointerException");}
		catch ( NullPointerException e )
		  {harness.check(true, "test_parseFloat null");}


		try {
			Float.parseFloat("Kona");
			harness.fail("test_parseFloat - 2" );
		}catch( NumberFormatException e)
		  {harness.check(true, "test_parseFloat Kona");}


		try {
			harness.check(!( Float.parseFloat( "3.4e+32f" ) != 3.4e+32f ),  
				"test_parseFloat - 3" );
		}catch( NumberFormatException e)
		  {harness.check(false, "test_parseFloat 3.4e+32f");}

		try {
			harness.check(!( Float.parseFloat(" -23.45f    ") != -23.45f ),  
				"test_parseFloat - 4" );
		}catch( NumberFormatException e)
		  {harness.check(false, "test_parseFloat \" -23.45f    \"");}

	}

	public void test_floatToIntBits()
	{
		harness.check(!( Float.floatToIntBits( Float.POSITIVE_INFINITY ) != 0x7f800000 ), 
			"test_floatToIntBits - 1" );
		harness.check(!( Float.floatToIntBits( Float.NEGATIVE_INFINITY ) != 0xff800000 ), 
			"test_floatToIntBits - 2" );
		
		int nanval = Float.floatToIntBits( Float.NaN ); 
		harness.check(!( nanval != 0x7fc00000 ), 
			"test_floatToIntBits - 3" );

		int i1 = Float.floatToIntBits(3.4e+32f);
		int i2 = Float.floatToIntBits(-34.56f);

		int sign1 = i1 & 0x80000000 ;
		int sign2 = i2 & 0x80000000 ;

		int exp1 = i1 & 0x7f800000 ;
		int exp2 = i2 & 0x7f800000 ;

		int man1 = i1 & 0x007fffff ;
		int man2 = i2 & 0x007fffff ;

		harness.check(!(sign1 != 0 ), 
			"test_floatToIntBits - 4" );

		harness.check(!( sign2 != 0x80000000 ), 
			"test_floatToIntBits - 5" );

		harness.check(!( exp1 != 1971322880 ), 
			"test_floatToIntBits - 6" );

		harness.check(!( exp2 != 1107296256 ), 
			"test_floatToIntBits - 7" );

		harness.check(!( man1 != 400186  ), 
			"test_floatToIntBits - 8" );

		harness.check(!( man2 != 671089 ), 
			"test_floatToIntBits - 9" );

	}

	public void test_intBitsToFloat( )
	{
		harness.check(!( Float.intBitsToFloat( 0x7f800000 ) != Float.POSITIVE_INFINITY ), 
			"test_intBitsToFloat - 1" );
		harness.check(!( Float.intBitsToFloat( 0xff800000 ) != Float.NEGATIVE_INFINITY ), 
			"test_intBitsToFloat - 2" );


		harness.check(!( !Float.isNaN(Float.intBitsToFloat( 0x7f800002 ))), 
			"test_intBitsToFloat - 3" );

		harness.check(!( !Float.isNaN(Float.intBitsToFloat( 0x7f8ffff0 ))), 
			"test_intBitsToFloat - 4" );

		harness.check(!( !Float.isNaN(Float.intBitsToFloat( 0xff800002 ) )), 
			"test_intBitsToFloat - 5" );

		harness.check(!( !Float.isNaN(Float.intBitsToFloat( 0xfffffff1 ))), 
			"test_intBitsToFloat - 6" );

		harness.check(!( !Float.isNaN(Float.intBitsToFloat( 0xffc00000 ))), 
			"test_intBitsToFloat - 7" );

		float fl1 = Float.intBitsToFloat( 0x34343f34 );
		
		harness.check(!( fl1 != 1.67868e-007f ), 
			"test_intBitsToFloat - 8" );

		harness.check(!( Float.floatToIntBits( Float.intBitsToFloat(0x33439943)) != 0x33439943 ), 
			"test_intBitsToFloat - 9");
	}

	public void test_shortbyteValue()
	{
		Float d1 = new Float( 123.35 );
		Float d2 = new Float( 400.35 );
		Float d3 = new Float(0.0 );
		
		harness.check(!( d1.shortValue() != 123 ), 
			"test_shortbyteValue - 1" );
		harness.check(!( d2.shortValue() != 400 ), 
			"test_shortbyteValue - 2" );
		harness.check(!( d3.shortValue() != 0 ), 
			"test_shortbyteValue - 3" );

		harness.check(!( d1.byteValue() != 123 ), 
			"test_shortbyteValue - 4" );
		harness.check(!( d2.byteValue() != (byte) 400 ), 
			"test_shortbyteValue - 5" );
		harness.check(!( d3.byteValue() != 0 ), 
			"test_shortbyteValue - 6" );
		
	}

	public void test_neg() {
	    float zero = 0.0f;
	    String zero1 = String.valueOf(zero);
	    if (!zero1.equals("0.0")) {
		harness.fail("test_neg - 1");
	    }

	    zero = -zero;
	    String zero2 = String.valueOf(zero);
	    if (!zero2.equals("-0.0")) {
		harness.fail("test_neg - 2");
	    }
	    
	    zero = -zero;
	    String zero3 = String.valueOf(zero);
	    if (!zero3.equals("0.0")) {
		harness.fail("test_neg - 3");
	    }
	    
	    float nonzero = -12.24f;
	    String nonzero1 = String.valueOf(nonzero);
	    if (!nonzero1.equals("-12.24")) {
		harness.fail("test_neg - 4");
	    }
	    
	    nonzero = -nonzero;
	    String nonzero2 = String.valueOf(nonzero);
	    if (!nonzero2.equals("12.24")) {
		harness.fail("test_neg - 5");
	    }
	    
	    nonzero = -nonzero;
	    String nonzero3 = String.valueOf(nonzero);
	    if (!nonzero3.equals("-12.24")) {
		harness.fail("test_neg - 6");
	    }
	}

	public void testall()
	{
		test_Basics();
		test_toString();
		test_equals();
		test_hashCode();
		test_intValue();
		test_longValue();
		test_floatValue();
		test_doubleValue();
		test_shortbyteValue();
		test_valueOf();
		test_parseFloat();
		test_floatToIntBits();
		test_intBitsToFloat();
		test_neg();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
