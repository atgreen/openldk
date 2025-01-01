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

package gnu.testlet.java.lang.Long;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class LongTest implements Testlet
{

  protected static TestHarness harness;
	public void test_Basics()
	{
		long min1 = Long.MIN_VALUE;
		long min2 = 0x8000000000000000L;
		long max1 = Long.MAX_VALUE;
		long max2 = 0x7fffffffffffffffL;

		harness.check(!( min1 != min2 || max1 != max2 ), 
			"test_Basics - 1" );

		Long i1 = new Long(100);

		harness.check(!( i1.longValue() != 100 ), 
			"test_Basics - 2" );

		try {
		harness.check(!( (new Long("234")).longValue() != 234 ), 
			"test_Basics - 3" );
		}
		catch ( NumberFormatException e )
		{
			harness.fail("test_Basics - 3" );
		}

		try {
		harness.check(!( (new Long("-FF")).longValue() != -255 ), 
			"test_Basics - 4" );
		}
		catch ( NumberFormatException e )
		{
		}

		try {
		    new Long("babu");
			harness.fail("test_Basics - 5" );
		}
		catch ( NumberFormatException e )
		{
		}
	}

	public void test_toString()
	{
		harness.check(!( !( new Long(123)).toString().equals("123")), 
			"test_toString - 1" );
		harness.check(!( !( new Long(-44)).toString().equals("-44")), 
			"test_toString - 2" );

		harness.check(!( !Long.toString( 234 ).equals ("234" )), 
			"test_toString - 3" );
		harness.check(!( !Long.toString( -34 ).equals ("-34" )), 
			"test_toString - 4" );
		harness.check(!( !Long.toString( -34 ).equals ("-34" )), 
			"test_toString - 5" );

		harness.check(!( !Long.toString(99 , 1 ).equals("99")), 
			"test_toString - 6" );
		harness.check(!( !Long.toString(99 , 37 ).equals("99")), 
			"test_toString - 7" );

		harness.check(!( !Long.toString(15 , 2 ).equals("1111")), 
			"test_toString - 8" );
		harness.check(!( !Long.toString(37 , 36 ).equals("11")), 
			"test_toString - 9" );
		harness.check(!( !Long.toString(31 , 16 ).equals("1f")), 
			"test_toString - 10" );


		harness.check(!( !Long.toString(-99 , 1 ).equals("-99")), 
			"test_toString - 11" );
		harness.check(!( !Long.toString(-99 , 37 ).equals("-99")), 
			"test_toString - 12" );

		harness.check(!( !Long.toString(-15 , 2 ).equals("-1111")), 
			"test_toString - 13" );
		harness.check(!( !Long.toString(-37 , 36 ).equals("-11")), 
			"test_toString - 14" );
		harness.check(!( !Long.toString(-31 , 16 ).equals("-1f")), 
			"test_toString - 15" );
	}

	public void test_equals()
	{
		Long i1 = new Long(23);
		Long i2 = new Long(-23);

		harness.check(!( !i1.equals( new Long(23))), 
			"test_equals - 1" );
		harness.check(!( !i2.equals( new Long(-23))), 
			"test_equals - 2" );

		
		harness.check(!( i1.equals( i2 )), 
			"test_equals - 3" );

		harness.check(!( i1.equals(null)), 
			"test_equals - 4" );
	}

	public void test_hashCode( )
	{
		Long b1 = new Long(34395555);
		Long b2 = new Long(-34395555);
		harness.check(!( b1.hashCode() != ((int)(b1.longValue()^(b1.longValue()>>>32)))  
			|| b2.hashCode() != ((int)(b2.longValue()^(b2.longValue()>>>32))) ), 
			"test_hashCode" );
	}

	public void test_intValue( )
	{
		Long b1 = new Long(32767);
		Long b2 = new Long(-32767);

		harness.check(!( b1.intValue() != 32767 ),  
			"test_intValue - 1" );

		harness.check(!( b2.intValue() != -32767 ),  
			"test_intValue - 2" );
	}
	public void test_shortbyteValue( )
	{
		Long b1 = new Long(32767);
		Long b2 = new Long(-32767);

		harness.check(!( b1.byteValue() != (byte)32767 ),  
			"test_shortbyteValue - 1" );

		harness.check(!( b2.byteValue() != (byte)-32767 ),  
			"test_shortbyteValue - 2" );
		harness.check(!( b1.shortValue() != (short)32767 ),  
			"test_shortbyteValue - 3" );

		harness.check(!( b2.shortValue() != (short)-32767 ),  
			"test_shortbyteValue - 4" );
	}

	public void test_longValue( )
	{
		Long b1 = new Long(-9223372036854775807L);
		Long b2 = new Long(9223372036854775807L);

		harness.check(!( b1.longValue() != (long)-9223372036854775807L ),  
			"test_longValue - 1" );

		harness.check(!( b2.longValue() != 9223372036854775807L ),  
			"test_longValue - 2" );
	}
	public void test_floatValue( )
	{
		Long b1 = new Long(3276);
		Long b2 = new Long(-3276);

		harness.check(!( b1.floatValue() != 3276.0f ),  
			"test_floatValue - 1" );

		harness.check(!( b2.floatValue() != -3276.0f ),  
			"test_floatValue - 2" );
	}
	public void test_doubleValue( )
	{
		Long b1 = new Long(0);
		Long b2 = new Long(30);

		harness.check(!( b1.doubleValue() != 0.0 ),  
			"test_doubleValue - 1" );

		harness.check(!( b2.doubleValue() != 30.0 ),  
			"test_doubleValue - 2" );
	}

	public void test_toHexString()
	{
		String str, str1;

		str = Long.toHexString(8375);
		str1 = Long.toHexString( -5361 ); 

		harness.check(!( !str.equalsIgnoreCase("20B7")), 
			"test_toHexString - 1" );

		harness.check(!( !str1.equalsIgnoreCase("ffffffffffffeb0f")), 
			"test_toHexString - 2" );	
	}

	public void test_toOctalString()
	{
		String str, str1;
		str = Long.toOctalString(5847);
		str1= Long.toOctalString(-9863 );

		harness.check(!( !str.equals("13327")), 
			"test_toOctalString - 1" );

		harness.check(!( !str1.equals("1777777777777777754571")), 
			"test_toOctalString - 2" );	
	}

	public void test_toBinaryString()
	{
		String str1 = Long.toBinaryString( -5478 ); 
		harness.check(!( !Long.toBinaryString(358).equals("101100110")), 
			"test_toBinaryString - 1" );

		harness.check(!( !str1.equals("1111111111111111111111111111111111111111111111111110101010011010")), 
			"test_toBinaryString - 2" );	
	}

	public void test_parseLong()
	{
		harness.check(!( Long.parseLong("473") != Long.parseLong("473" , 10 )), 
			"test_parseLong - 1" );	

		harness.check(!( Long.parseLong("0" , 10 ) != 0L ),  
			"test_parseLong - 2" );	

		harness.check(!( Long.parseLong("473" , 10 ) != 473L ),  
			"test_parseLong - 3" );	
		harness.check(!( Long.parseLong("-0" , 10 ) != 0L ),  
			"test_parseLong - 4" );	
		harness.check(!( Long.parseLong("-FF" , 16 ) != -255L ),  
			"test_parseLong - 5" );	
		harness.check(!( Long.parseLong("1100110" , 2 ) != 102L ),  
			"test_parseLong - 6" );	
		harness.check(!( Long.parseLong("2147483647" , 10 )  !=  2147483647L ),  
			"test_parseLong - 7" );	
		harness.check(!( Long.parseLong("-2147483647" , 10 ) != -2147483647L ),  
			"test_parseLong - 8" );	

		try {
			Long.parseLong("99" , 8 );
			harness.fail("test_parseLong - 10" );	
		}catch ( NumberFormatException e ){}

		try {
			Long.parseLong("Hazelnut" , 10 );
			harness.fail("test_parseLong - 11" );	
		}catch ( NumberFormatException e ){}

        	harness.check(!( Long.parseLong("Hazelnut" , 36 ) != 1356099454469L ), 
			"test_parseLong - 12" );	


        	long_hex_ok("-8000000000000000", -0x8000000000000000L);
        	long_hex_ok("7fffffffffffffff", 0x7fffffffffffffffL);
        	long_hex_ok("7ffffffffffffff3", 0x7ffffffffffffff3L);
        	long_hex_ok("7ffffffffffffffe", 0x7ffffffffffffffeL);
        	long_hex_ok("7ffffffffffffff0", 0x7ffffffffffffff0L);

        	long_hex_bad("80000000000000010");
        	long_hex_bad("7ffffffffffffffff");
        	long_hex_bad("8000000000000001");
        	long_hex_bad("8000000000000002");
	        long_hex_bad("ffffffffffffffff");
       		long_hex_bad("-8000000000000001");
        	long_hex_bad("-8000000000000002");

        	long_dec_ok("-9223372036854775808", -9223372036854775808L);
        	long_dec_ok("-9223372036854775807", -9223372036854775807L);
        	long_dec_ok("-9223372036854775806", -9223372036854775806L);
 		long_dec_ok("9223372036854775807", 9223372036854775807L);
        	long_dec_ok("9223372036854775806", 9223372036854775806L);
        	long_dec_bad("-9223372036854775809");
       		long_dec_bad("-9223372036854775810");
        	long_dec_bad("-9223372036854775811");
        	long_dec_bad("9223372036854775808");
	}

   	static void long_hex_ok ( String s, long ref ) {
        	try {
           		long l = Long.parseLong(s, 16);
            		if (ref != l) {
				System.out.println(
				" Error : long_hex_ok failed - 1 " +
				" expected " + ref + " actual " + l );
			}
		}
		catch ( NumberFormatException e )
		{
			System.out.println(
			       " Error : long_hex_ok failed - 2 " +
			       " should not have thrown exception in parsing" + 
			       s );
		}
	}

    	static void long_hex_bad(String s) {
       		try {
            		Long.parseLong(s, 16);
            		harness.fail("long_hex_bad "  +
				s +  " should not be valid!" );
       		} catch (NumberFormatException e ){
		}
	}

   	static void long_dec_ok(String s, long ref) {
       		try {
            		long l = Long.parseLong(s, 10);
            		if (ref != l) {
				System.out.println(
				" Error : long_dec_ok failed - 1 " +
				" expected " + ref + " actual " + l );
			}
		}
		catch ( NumberFormatException e )
		{
			System.out.println(
			       " Error : long_dec_ok failed - 2 " +
			       " should not have thrown exception in parsing" + 
			       s );
		}
	}

    	static void long_dec_bad(String s) {
       		try {
            		Long.parseLong(s, 10);
            		System.out.println(" Error long_dec_bad failed for"+s );
        	} catch (NumberFormatException e ){}
	}


	public void test_valueOf( )
	{
		harness.check(!( Long.valueOf("21234").longValue() != Long.parseLong("21234")), 
			"test_valueOf - 1" );	
		harness.check(!( Long.valueOf("Kona", 27).longValue() != Long.parseLong("Kona", 27)), 
			"test_valueOf - 2" );	
	}

	public void test_getLong( )
	{
		java.util.Properties  prop = System.getProperties();
		prop.put("longkey1" , "2345" );
		prop.put("longkey2" , "-984" );
		prop.put("longkey3" , "-0" );

		prop.put("longkey4" , "#1f" );
		prop.put("longkey5" , "0x1f" );
		prop.put("longkey6" , "017" );

		prop.put("longkey7" , "babu" );



		System.setProperties(prop);
		harness.checkPoint("getLong");
		try {
		  harness.check(Long.getLong("longkey1").longValue() == 2345);
		} catch (NullPointerException npe) { harness.check(false); }
		try {
		  harness.check(Long.getLong("longkey2").longValue() == -984);
		} catch (NullPointerException npe) { harness.check(false); }
		try {
		  harness.check(Long.getLong("longkey3").longValue() == 0);
		} catch (NullPointerException npe) { harness.check(false); }

		try {
		  harness.check(Long.getLong("longkey4",new Long(0)).longValue() == 31);
		} catch (NullPointerException npe) { harness.check(false); }
		try {
		  harness.check(Long.getLong("longkey5",new Long(0)).longValue() == 31);
		} catch (NullPointerException npe) { harness.check(false); }
		try {
		  harness.check(Long.getLong("longkey6",new Long(0)).longValue() == 15);
		} catch (NullPointerException npe) { harness.check(false); }

		try {
		  harness.check(!( Long.getLong("longkey7", new Long(0)).longValue() != 0 ), 
			"test_getLong - 3" );
		} catch (NullPointerException npe) { harness.check(false); }
		try {
		  harness.check(!( Long.getLong("longkey7", 0).longValue() != 0 ), 
			"test_getLong - 4" );
		} catch (NullPointerException npe) { harness.check(false); }

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
		test_toHexString();
		test_toOctalString();
		test_toBinaryString();
		test_parseLong();
		test_valueOf();
		test_getLong();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
