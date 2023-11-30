/* Copyright (C) 1999, 2001 Hewlett-Packard Company

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

package gnu.testlet.java.lang.Integer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class IntegerTest implements Testlet
{

  protected static TestHarness harness;
	public void test_Basics()
	{
		harness.check(!( Integer.MIN_VALUE != 0x80000000 || 
			 Integer.MAX_VALUE != 0x7fffffff ), 
			"test_Basics - 1" );

		harness.check(Integer.TYPE
			      == new int[0].getClass().getComponentType(),
			      "test_Basics - 1a");

		Integer i1 = new Integer(100);

		harness.check(!( i1.intValue() != 100 ), 
			"test_Basics - 2" );

		try {
		harness.check(!( (new Integer("234")).intValue() != 234 ), 
			"test_Basics - 3" );
		}
		catch ( NumberFormatException e )
		{
			harness.fail("test_Basics - 3" );
		}

		try {
		harness.check(!( (new Integer("-FF")).intValue() != -255 ), 
			"test_Basics - 4" );
		}
		catch ( NumberFormatException e )
		{
		}

		try {
		    new Integer("babu");
			harness.fail("test_Basics - 5" );
		}
		catch ( NumberFormatException e )
		{
		}
		harness.check(!( Integer.decode( "123").intValue() != 123 ), 
			"test_Basics - 6" );
		harness.check(!( Integer.decode( "32767").intValue() != 32767 ), 
			"test_Basics - 7" );

	}

	public void test_toString()
	{
		harness.check(!( !( new Integer(123)).toString().equals("123")), 
			"test_toString - 1" );
		harness.check(!( !( new Integer(-44)).toString().equals("-44")), 
			"test_toString - 2" );

		harness.check(!( !Integer.toString( 234 ).equals ("234" )), 
			"test_toString - 3" );
		harness.check(!( !Integer.toString( -34 ).equals ("-34" )), 
			"test_toString - 4" );
		harness.check(!( !Integer.toString( -34 ).equals ("-34" )), 
			"test_toString - 5" );

		harness.check(!( !Integer.toString(99 , 1 ).equals("99")), 
			"test_toString - 6" );
		harness.check(!( !Integer.toString(99 , 37 ).equals("99")), 
			"test_toString - 7" );

		harness.check(!( !Integer.toString(15 , 2 ).equals("1111")), 
			"test_toString - 8" );
		harness.check(!( !Integer.toString(37 , 36 ).equals("11")), 
			"test_toString - 9" );
		harness.check(!( !Integer.toString(31 , 16 ).equals("1f")), 
			"test_toString - 10" );


		harness.check(!( !Integer.toString(-99 , 1 ).equals("-99")), 
			"test_toString - 11" );
		harness.check(!( !Integer.toString(-99 , 37 ).equals("-99")), 
			"test_toString - 12" );

		harness.check(!( !Integer.toString(-15 , 2 ).equals("-1111")), 
			"test_toString - 13" );
		harness.check(!( !Integer.toString(-37 , 36 ).equals("-11")), 
			"test_toString - 14" );
		harness.check(!( !Integer.toString(-31 , 16 ).equals("-1f")), 
			"test_toString - 15" );
	}

	public void test_equals()
	{
		Integer i1 = new Integer(23);
		Integer i2 = new Integer(-23);

		harness.check(!( !i1.equals( new Integer(23))), 
			"test_equals - 1" );
		harness.check(!( !i2.equals( new Integer(-23))), 
			"test_equals - 2" );

		
		harness.check(!( i1.equals( i2 )), 
			"test_equals - 3" );

		harness.check(!( i1.equals(null)), 
			"test_equals - 4" );
	}

	public void test_hashCode( )
	{
		Integer b1 = new Integer(3439);
		Integer b2 = new Integer(-3439);

		harness.check(!( b1.hashCode() != 3439 || b2.hashCode() != -3439 ), 
			"test_hashCode" );
	}

	public void test_intValue( )
	{
		Integer b1 = new Integer(32767);
		Integer b2 = new Integer(-32767);

		harness.check(!( b1.intValue() != 32767 ),  
			"test_intValue - 1" );

		harness.check(!( b2.intValue() != -32767 ),  
			"test_intValue - 2" );
	}

	public void test_longValue( )
	{
		Integer b1 = new Integer(3767);
		Integer b2 = new Integer(-3767);

		harness.check(!( b1.longValue() != (long)3767 ),  
			"test_longValue - 1" );

		harness.check(!( b2.longValue() != -3767 ),  
			"test_longValue - 2" );
	}
	public void test_floatValue( )
	{
		Integer b1 = new Integer(3276);
		Integer b2 = new Integer(-3276);

		harness.check(!( b1.floatValue() != 3276.0f ),  
			"test_floatValue - 1" );

		harness.check(!( b2.floatValue() != -3276.0f ),  
			"test_floatValue - 2" );
	}
	public void test_doubleValue( )
	{
		Integer b1 = new Integer(0);
		Integer b2 = new Integer(30);

		harness.check(!( b1.doubleValue() != 0.0 ),  
			"test_doubleValue - 1" );

		harness.check(!( b2.doubleValue() != 30.0 ),  
			"test_doubleValue - 2" );
	}

	public void test_shortbyteValue( )
	{
		Integer b1 = new Integer(0);
		Integer b2 = new Integer(300);

		harness.check(!( b1.byteValue() != 0 ),  
			"test_shortbyteValue - 1" );

		harness.check(!( b2.byteValue() != (byte)300 ),  
			"test_shortbyteValue - 2" );
		harness.check(!( b1.shortValue() != 0 ),  
			"test_shortbyteValue - 3" );

		harness.check(!( b2.shortValue() != (short)300 ),  
			"test_shortbyteValue - 4" );
		harness.check(!( ((Number)b1).shortValue() != 0 ),  
			"test_shortbyteValue - 5" );

		harness.check(!( ((Number)b2).byteValue() != (byte)300 ),  
			"test_shortbyteValue - 6" );
	}

	public void test_toHexString()
	{
		String str, str1;

		str = Integer.toHexString(8375);
		str1 = Integer.toHexString( -5361 ); 

		harness.check( "20b7".equals(str), 
			"test_toHexString - 1" );

		harness.check( "ffffeb0f".equals(str1),
			"test_toHexString - 2" );	
	}

	public void test_toOctalString()
	{
		String str, str1;
		str = Integer.toOctalString(5847);
		str1= Integer.toOctalString(-9863 );

		harness.check(!( !str.equals("13327")), 
			"test_toOctalString - 1" );

		harness.check(!( !str1.equals("37777754571")), 
			"test_toOctalString - 2" );	
	}

	public void test_toBinaryString()
	{
		harness.check(!( !Integer.toBinaryString(358).equals("101100110")), 
			"test_toBinaryString - 1" );

		harness.check(!( !Integer.toBinaryString( -5478 ).equals("11111111111111111110101010011010")), 
			"test_toBinaryString - 2" );	
	}

	public void test_parseInt()
	{
		harness.check(!( Integer.parseInt("473") != Integer.parseInt("473" , 10 )), 
			"test_parseInt - 1" );	

		harness.check(!( Integer.parseInt("0" , 10 ) != 0 ),  
			"test_parseInt - 2" );	

		harness.check(!( Integer.parseInt("473" , 10 ) != 473 ),  
			"test_parseInt - 3" );	
		harness.check(!( Integer.parseInt("-0" , 10 ) != 0 ),  
			"test_parseInt - 4" );	
		harness.check(!( Integer.parseInt("-FF" , 16 ) != -255 ),  
			"test_parseInt - 5" );	
		harness.check(!( Integer.parseInt("1100110" , 2 ) != 102 ),  
			"test_parseInt - 6" );	
		harness.check(!( Integer.parseInt("2147483647" , 10 )  !=  2147483647 ),  
			"test_parseInt - 7" );	
		harness.check(!( Integer.parseInt("-2147483647" , 10 ) != -2147483647 ),  
			"test_parseInt - 8" );	
		try {
			Integer.parseInt("2147483648" , 10 );
			harness.fail("test_parseInt - 9" );	
		}catch ( NumberFormatException e ){}
		try {
			Integer.parseInt("99" , 8 );
			harness.fail("test_parseInt - 10" );	
		}catch ( NumberFormatException e ){}
		try {
			Integer.parseInt("kona" , 10 );
			harness.fail("test_parseInt - 11" );	
		}catch ( NumberFormatException e ){}
        harness.check(!( Integer.parseInt("Kona" , 27 ) != 411787 ), 
			"test_parseInt - 12" );	
	}

	public void test_valueOf( )
	{
		harness.check(!( Integer.valueOf("21234").intValue() != Integer.parseInt("21234")), 
			"test_valueOf - 1" );	
		harness.check(!( Integer.valueOf("Kona", 27).intValue() != Integer.parseInt("Kona", 27)), 
			"test_valueOf - 2" );	
	}

	public void test_getInteger( )
	{
		java.util.Properties  prop = System.getProperties();
		prop.put("integerkey1" , "2345" );
		prop.put("integerkey2" , "-984" );
		prop.put("integerkey3" , "-0" );

		prop.put("integerkey4" , "#1f" );
		prop.put("integerkey5" , "0x1f" );
		prop.put("integerkey6" , "017" );

		prop.put("integerkey7" , "babu" );



		System.setProperties(prop);

		harness.check(!( Integer.getInteger("integerkey1").intValue() != 2345 ||
			 Integer.getInteger("integerkey2").intValue() != -984 ||
			 Integer.getInteger("integerkey3").intValue() != 0 ), 
			"test_getInteger - 1" );

		harness.check(!( Integer.getInteger("integerkey4", new Integer(0)).intValue() != 31 ||
			 Integer.getInteger("integerkey5",new Integer(0)).intValue() != 31 ||
			 Integer.getInteger("integerkey6",new Integer(0)).intValue() != 15 ), 
			"test_getInteger - 2" );

		harness.check(!( Integer.getInteger("integerkey7", new Integer(0)).intValue() != 0 ), 
			"test_getInteger - 3" );
		harness.check(!( Integer.getInteger("integerkey7", 0).intValue() != 0 ), 
			"test_getInteger - 4" );

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
		test_parseInt();
		test_valueOf();
		test_getInteger();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
