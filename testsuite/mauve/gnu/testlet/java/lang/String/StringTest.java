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

package gnu.testlet.java.lang.String;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class StringTest implements Testlet
{

  protected static TestHarness harness;
	public void test_Basics()
	{
		String str1 = new String();
		harness.check(!( str1.length() != 0 ),  
			"test_Basics - 1");
		harness.check(!( !str1.toString().equals("")), 
			"test_Basics - 2");

		String str2 = new String("testing" );
		harness.check(!( str2.length() != 7 ),  
			"test_Basics - 3");
		harness.check(!( !str2.toString().equals("testing")), 
			"test_Basics - 4");

		
		try {
			String str = null;
			String str3 = new String(str);
			harness.fail("test_Basics - 5");
		}
		catch ( NullPointerException e ){}	

		String str4 = new String( new StringBuffer("hi there"));
		harness.check(!( str4.length () != 8 ),  
			"test_Basics - 6");
		harness.check(!( !str4.toString().equals("hi there")), 
			"test_Basics - 7");

		char cdata[] = { 'h' , 'e' , 'l' , 'l' , 'o' };
		String str5 = new String( cdata );
		harness.check(!( str5.length () != 5 ),  
			"test_Basics - 8");
		harness.check(!( !str5.toString().equals("hello")), 
			"test_Basics - 9");

		try {
			String str6 = new String( cdata , 0 , 10 );
			harness.fail("test_Basics - 10");

		}catch ( IndexOutOfBoundsException e )
		{}

		try {
			byte [] barr = null;
			String str7 = new String( barr , 0 , 10 );
			harness.fail("test_Basics - 11");

		}catch ( NullPointerException e )
		{}

		String str8 = new String( cdata , 0 , 4 );
		harness.check(!( !str8.equals("hell")), 
			"test_Basics - 12");

		try {
			String str10 = new String( null , 10 );
			harness.fail("test_Basics - 13");

		}catch ( NullPointerException e )
		{}

		byte bdata[] = { (byte)'d',(byte)'a',(byte)'n',(byte)'c',(byte)'i',(byte)'n',(byte)'g' };
		String str9 = new String(bdata , 10 );

		char ch = str9.charAt(1);
		int i = (ch & 0xff00 ) >> 8 ;
		byte b = (byte)(ch & 0x00ff );

		harness.check(!( i != 10 ||  b != 'a' ), 
			"test_Basics - 14");
		

		byte bnull [] = null;
		try {
			String str11 = new String( bnull , 10 , 0 , 5);
			harness.fail("test_Basics - 15");
		}catch ( NullPointerException e ){}

		try {
			String str12 = new String( bdata , 10 , -1 , 3);
			harness.fail("test_Basics - 16");
		}catch ( IndexOutOfBoundsException  e ){}


		String str13 = new String( bdata , 10 , 1 , 1 );
		i = (ch & 0xff00 ) >> 8 ;
		b = (byte)(ch & 0x00ff );

		harness.check(!( i != 10 ||  b != 'a' ), 
			"test_Basics - 17");

		String str14 = new String( bdata);
		harness.check(!( !str14.equals("dancing")), 
			"test_Basics - 18");

		// EJWcr00461
		byte arr[]={(byte)'a'};
		String str15 = new String(arr,0x1234,0,1);
		if (!str15.equals("\u3461")) {
		    harness.fail("test_Basics - 19");
		}

		// EJWcr00462
		char carr[] = {'h','e','l','l','o'};
		try {
		    String str16 = new String(carr, Integer.MAX_VALUE, 1);
		    harness.fail("test_Basics - 20");
		} catch (IndexOutOfBoundsException e) {
		}
		byte arr2[]={(byte)'a', (byte)'b', (byte)'c', (byte)'d', (byte)'e'};
		try {
		    String str17 = new String(arr2,0x1234,Integer.MAX_VALUE,1);
		    harness.fail("test_Basics - 21");
		} catch (IndexOutOfBoundsException e) {
		}
				 
		// this used to cause the vm to core dump (cr543)
		String s = "\u0d3e";

	}

	public void test_toString()
	{
		String str1 = "218943289";

		harness.check(!( !str1.toString().equals("218943289")), 
			"test_toString - 1");

		harness.check(!( str1 != "218943289" ), 
			"test_toString - 2");

		harness.check(!( !str1.equals(str1.toString())), 
			"test_toString - 3");		
	}

	public void test_equals()
	{
		String str2 = new String("Nectar");

		harness.check(!( str2.equals( null )), 
			"test_equals - 1");		

		harness.check(!( !str2.equals("Nectar")), 
			"test_equals - 2");		

		harness.check(!( str2.equals("")), 
			"test_equals - 3");		

		harness.check(!( str2.equals("nectar")), 
			"test_equals - 4");		

		harness.check(!( !"".equals("")), 
			"test_equals - 5");		

	}

	public void test_hashCode()
	{
		String str1 = "hp";
		String str2 = "Hewlett Packard Company";

		int hash1 = 'h' * 31 + 'p';
		int acthash1 = str1.hashCode(); 

		harness.check(!( hash1 != acthash1 ), 
			"test_hashCode - 1");		
	}

	public void test_length()
	{
		harness.check(!( "".length() != 0 ),  
			"test_length - 1");
		
		harness.check(!( "pentium".length() != 7 ),  
			"test_length - 2");
	}

	public void test_charAt()
	{
		harness.check(!( "abcd".charAt(0) != 'a' || "abcd".charAt(1) != 'b' ||
			 "abcd".charAt(2) != 'c' || "abcd".charAt(3) != 'd'	), 
			"test_charAt - 1");

		try {
			char ch = "abcd".charAt(4);
			harness.fail("test_charAt - 2");
		}
		catch ( IndexOutOfBoundsException e ){}

		try {
			char ch = "abcd".charAt(-1);
			harness.fail("test_charAt - 3");
		}
		catch ( IndexOutOfBoundsException e ){}
	}

	public void test_getChars()
	{
		String str = "abcdefghijklmn";

		try {
			str.getChars(0 , 3 , null , 1 );
			harness.fail("test_getChars - 1");
		}catch ( NullPointerException e ){}

		char dst[] = new char[5];
		
		try {
			str.getChars(-1 , 3 , dst , 1 );
			harness.fail("test_getChars - 2");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(4 , 3 , dst , 1 );
			harness.fail("test_getChars - 3");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(1 , 15 , dst , 1 );
			harness.fail("test_getChars - 4");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(1 , 5 , dst , -1 );
			harness.fail("test_getChars - 5");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getChars(1 , 10 , dst , 1 );
			harness.fail("test_getChars - 6");
		}catch ( IndexOutOfBoundsException e ){}

		str.getChars(0,5,dst, 0 );
		harness.check(!( dst[0] != 'a' || dst[1] != 'b' || dst[2] != 'c' ||
			 				  dst[3] != 'd' || dst[4] != 'e' ), 
			"test_getChars - 7");

		dst[0] = dst[1] = dst[2] = dst[3] = dst[4] = ' ';
		str.getChars(0,0,dst, 0 );
		harness.check(!( dst[0] != ' ' || dst[1] != ' ' || dst[2] != ' ' ||
			 				  dst[3] != ' ' || dst[4] != ' ' ), 
			"test_getChars - 9");

		dst[0] = dst[1] = dst[2] = dst[3] = dst[4] = ' ';
		str.getChars(0,1,dst, 0 );
		harness.check(!( dst[0] != 'a' || dst[1] != ' ' || dst[2] != ' ' ||
			 				  dst[3] != ' ' || dst[4] != ' ' ), 
			"test_getChars - 10");
	}


	public void test_getBytes()
	{
		String str = "abcdefghijklmn";

		try {
			str.getBytes(0 , 3 , null , 1 );
			harness.fail("test_getBytes - 1");
		}catch ( NullPointerException e ){}

		byte dst[] = new byte[5];
		
		try {
			str.getBytes(-1 , 3 , dst , 1 );
			harness.fail("test_getBytes - 2");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getBytes(4 , 3 , dst , 1 );
			harness.fail("test_getBytes - 3");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getBytes(1 , 15 , dst , 1 );
			harness.fail("test_getBytes - 4");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getBytes(1 , 5 , dst , -1 );
			harness.fail("test_getBytes - 5");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			str.getBytes(1 , 10 , dst , 1 );
			harness.fail("test_getBytes - 6");
		}catch ( IndexOutOfBoundsException e ){}

		str.getBytes(0,5,dst, 0 );
		harness.check(!( dst[0] != 'a' || dst[1] != 'b' || dst[2] != 'c' ||
			 				  dst[3] != 'd' || dst[4] != 'e' ), 
			"test_getBytes - 7");

		byte [] dst1 = new byte[40];
		dst1 = str.getBytes();
		harness.check(!( dst1[0] != 'a' || dst1[1] != 'b' || dst1[2] != 'c' ||
			 				  dst1[3] != 'd' || dst1[4] != 'e' ), 
			"test_getBytes - 8");
	}

	public void test_toCharArray()
	{
		char[] charr = "abcde".toCharArray();

		harness.check(!( charr[0] != 'a' || charr[1] != 'b' ||
			charr[2] != 'c' || charr[3] != 'd' ||
			charr[4] != 'e' ), 
			"test_toCharArray - 1");

		char [] charr1 = "".toCharArray();

		harness.check(!( charr1.length  > 0 ), 
			"test_toCharArray - 2");
	}

	public void test_equalsIgnoreCase()
	{
		harness.check(!( "hi".equalsIgnoreCase(null)), 
			"test_equalsIgnoreCase - 1");

		harness.check(!( !"hi".equalsIgnoreCase("HI")), 
			"test_equalsIgnoreCase - 2");

		harness.check(!( "hi".equalsIgnoreCase("pq")), 
			"test_equalsIgnoreCase - 3");

		harness.check(!( "hi".equalsIgnoreCase("HI ")), 
			"test_equalsIgnoreCase - 4");

	}

	public void test_compareTo()
	{
		try {
			int res = "abc".compareTo(null);
			harness.fail("test_compareTo - 1");
		}
		catch ( NullPointerException e ){}

		harness.check(!( "abc".compareTo("bcdef") >= 0  ), 
			"test_compareTo - 2");

		harness.check(!( "abc".compareTo("abc") != 0 ), 
			"test_compareTo - 3");

		harness.check(!( "abc".compareTo("aabc") <= 0 ), 
			"test_compareTo - 4");

		harness.check(!( "abcd".compareTo("abc") <= 0 ), 
			"test_compareTo - 5");

		harness.check(!( "".compareTo("abc") >= 0 ), 
			"test_compareTo - 6");
	}

	public void test_regionMatches()
	{
		try {
			boolean res = "abc".regionMatches(0 , null , 0 , 2);
			harness.fail("test_regionMatches - 1");
		}
		catch ( NullPointerException e ){}

		harness.check(!( "abcd".regionMatches(-1 , "abcd" , 0 , 2 )), 
			"test_regionMatches - 2");
		harness.check(!( "abcd".regionMatches(0 , "abcd" , - 1 , 2 )), 
			"test_regionMatches - 3");
		harness.check(!( "abcd".regionMatches(0 , "abcd" , 0 , 10 )), 
			"test_regionMatches - 4");
		harness.check(!( "abcd".regionMatches(0 , "ab" , 0 , 3 )), 
			"test_regionMatches - 5");

		harness.check(!( !"abcd".regionMatches(1 , "abc" , 1 , 2 )), 
			"test_regionMatches - 6");

		harness.check(!( !"abcd".regionMatches(1 , "abc" , 1 , 0 )), 
			"test_regionMatches - 7");

		harness.check(!( "abcd".regionMatches(1 , "ABC" , 1 , 2 )), 
			"test_regionMatches - 8");
		

		try {
			boolean res = "abc".regionMatches(true , 0 , null , 0 , 2);
			harness.fail("test_regionMatches - 11");
		}
		catch ( NullPointerException e ){}

		harness.check(!( "abcd".regionMatches(true , -1 , "abcd" , 0 , 2 )), 
			"test_regionMatches - 12");
		harness.check(!( "abcd".regionMatches(true , 0 , "abcd" , - 1 , 2 )), 
			"test_regionMatches - 13");
		harness.check(!( "abcd".regionMatches(true , 0 , "abcd" , 0 , 10 )), 
			"test_regionMatches - 14");
		harness.check(!( "abcd".regionMatches(true , 0 , "ab" , 0 , 3 )), 
			"test_regionMatches - 15");

		harness.check(!( !"abcd".regionMatches(true , 1 , "abc" , 1 , 2 )), 
			"test_regionMatches - 16");

		harness.check(!( !"abcd".regionMatches(true , 1 , "abc" , 1 , 0 )), 
			"test_regionMatches - 17");

		harness.check(!( !"abcd".regionMatches(true , 1 , "ABC" , 1 , 2 )), 
			"test_regionMatches - 18");
		harness.check(!( "abcd".regionMatches(false , 1 , "ABC" , 1 , 2 )), 
			"test_regionMatches - 19");
	}

	public void test_startsWith()
	{
		harness.check(!( !"abcdef".startsWith( "abc")), 
			"test_startsWith - 1");

		try {
			boolean b = "abcdef".startsWith( null );
			harness.fail("test_startsWith - 2");
		} catch ( NullPointerException e ){}

		harness.check(!( "abcdef".startsWith( "ABC")), 
			"test_startsWith - 3");

		harness.check(!( !"abcdef".startsWith( "")), 
			"test_startsWith - 4");

		harness.check(!( "abc".startsWith( "abcd")), 
			"test_startsWith - 5");


		harness.check(!( !"abcdef".startsWith( "abc" , 0 )), 
			"test_startsWith - 6");

		try {
			boolean b = "abcdef".startsWith( null ,0);
			harness.fail("test_startsWith - 7");
		} catch ( NullPointerException e ){}

		harness.check(!( "abcdef".startsWith( "ABC", 2)), 
			"test_startsWith - 8");

		harness.check(!( !"abcdef".startsWith( "", 0 )), 
			"test_startsWith - 9");

		harness.check(!( "abc".startsWith( "abcd" , 3)), 
			"test_startsWith - 10");

		harness.check(!( "abc".startsWith( "abc" , 10)), 
			"test_startsWith - 11");
	}

	public void test_endsWith()
	{
		harness.check(!( !"abcdef".endsWith( "def")), 
			"test_endsWith - 1");

		try {
			boolean b = "abcdef".endsWith( null );
			harness.fail("test_endsWith - 2");
		} catch ( NullPointerException e ){}

		harness.check(!( "abcdef".endsWith( "DEF")), 
			"test_endsWith - 3");

		harness.check(!( !"abcdef".endsWith( "")), 
			"test_endsWith - 4");

		harness.check(!( "bcde".endsWith( "abcd")), 
			"test_endsWith - 5");

	}

	public void test_indexOf()
	{
		harness.check(!( "a".indexOf('a') != 0 ), 
			"test_indexOf - 1");

		harness.check(!( "aabc".indexOf('c') != 3 ), 
			"test_indexOf - 2");

		harness.check(!( "a".indexOf('c') != -1 ), 
			"test_indexOf - 3");

		harness.check(!( "".indexOf('a') != -1 ), 
			"test_indexOf - 4");


		harness.check(!( "abcde".indexOf('b', 3) != -1 ), 
			"test_indexOf - 5");
		harness.check(!( "abcde".indexOf('b', 0) != 1 ), 
			"test_indexOf - 6");
		harness.check(!( "abcdee".indexOf('e', 3) != 4 ), 
			"test_indexOf - 7");
		harness.check(!( "abcdee".indexOf('e', 5) != 5 ), 
			"test_indexOf - 8");

		harness.check(!( "abcdee".indexOf('e', -5) != 4 ), 
			"test_indexOf - 9");
		harness.check(!( "abcdee".indexOf('e', 15) != -1 ), 
			"test_indexOf - 10");


		harness.check(!( "abcdee".indexOf("babu") != -1 ), 
			"test_indexOf - 11");
		try {
			int x = "abcdee".indexOf(null);
		   	harness.fail("test_indexOf - 12");
		}catch ( NullPointerException e ){} 
	
		harness.check(!( "abcdee".indexOf("") != 0 ), 
			"test_indexOf - 13");
		harness.check(!( "abcdee".indexOf("ee") != 4 ), 
			"test_indexOf - 14");
		harness.check(!( "abcbcbc".indexOf("cbc") != 2 ), 
			"test_indexOf - 15");

		harness.check(!( "abcdee".indexOf("babu", 3) != -1 ), 
			"test_indexOf - 16");
		try {
			int x = "abcdee".indexOf(null,0);
		   	harness.fail("test_indexOf - 17");
		}catch ( NullPointerException e ){} 
	
		harness.check(!( "abcdee".indexOf("", 0) != 0 ), 
			"test_indexOf - 18");
		harness.check(!( "abcdee".indexOf("ee", 4) != 4 ), 
			"test_indexOf - 19");
		harness.check(!( "abcbcbc".indexOf("cbc",4 ) != 4 ), 
			"test_indexOf - 20");
		// EJWcr00463
		if ( "hello \u5236 world".indexOf('\u5236') != 6 ) {
			harness.fail("test_indexOf - 21");
		}
		if ( "hello \u0645 world".indexOf('\u0645') != 6 ) {
			harness.fail("test_indexOf - 22");
		}
		if ( "hello \u07ff world".indexOf('\u07ff') != 6 ) {
			harness.fail("test_indexOf - 23");
		}
	}

	public void test_lastIndexOf()
	{
		harness.check(!( "a".lastIndexOf('a') != 0 ), 
			"test_lastIndexOf - 1");

		harness.check(!( "aabc".lastIndexOf('c') != 3 ), 
			"test_lastIndexOf - 2");

		harness.check(!( "a".lastIndexOf('c') != -1 ), 
			"test_lastIndexOf - 3");

		harness.check(!( "".lastIndexOf('a') != -1 ), 
			"test_lastIndexOf - 4");


		harness.check(!( "abcde".lastIndexOf('b', 0) != -1 ), 
			"test_lastIndexOf - 5");
		harness.check(!( "abcde".lastIndexOf('b', 4) != 1 ), 
			"test_lastIndexOf - 6");
		harness.check(!( "abcdee".lastIndexOf('e', 7) != 5 ), 
			"test_lastIndexOf - 7");
		harness.check(!( "abcdee".lastIndexOf('e', 5) != 5 ), 
			"test_lastIndexOf - 8");

		harness.check(!( "abcdee".lastIndexOf('e', -5) != -1 ), 
			"test_lastIndexOf - 9");
		harness.check(!( "abcdee".lastIndexOf('e', 15) != 5 ), 
			"test_lastIndexOf - 10");


		harness.check(!( "abcdee".lastIndexOf("babu") != -1 ), 
			"test_lastIndexOf - 11");
		try {
			int x = "abcdee".lastIndexOf(null);
		   	harness.fail("test_lastIndexOf - 12");
		}catch ( NullPointerException e ){} 
	
		harness.check(!( "abcdee".lastIndexOf("") != 6 ), 
			"test_lastIndexOf - 13");
		harness.check(!( "abcdee".lastIndexOf("ee") != 4 ), 
			"test_lastIndexOf - 14");
		harness.check(!( "abcbcbc".lastIndexOf("cbc") != 4 ), 
			"test_lastIndexOf - 15");

		harness.check(!( "abcdee".lastIndexOf("babu", 3) != -1 ), 
			"test_lastIndexOf - 16");

		try {
			int x = "abcdee".lastIndexOf(null,0);
		   	harness.fail("test_lastIndexOf - 17");
		}catch ( NullPointerException e ){} 
	
		harness.check(!( "abcdee".lastIndexOf("", 0) != 0 ), 
			"test_lastIndexOf - 18");
		harness.check(!( "abcdee".lastIndexOf("ee", 4) != 4 ), 
			"test_lastIndexOf - 19");
		harness.check(!( "abcbcbc".lastIndexOf("cbc",3 ) != 2 ), 
			"test_lastIndexOf - 20");
	}

	public void test_substring()
	{
		harness.check(!( !"unhappy".substring(2).equals("happy")), 
			"test_substring - 1");
		harness.check(!( !"Harbison".substring(3).equals("bison")), 
			"test_substring - 2");
		harness.check(!( !"emptiness".substring(9).equals("")), 
			"test_substring - 3");

		try {
			String str = "hi there".substring(-1);
			harness.fail("test_substring - 4");
		}catch( IndexOutOfBoundsException e ){}

		try {
			String str = "hi there".substring(10);
			harness.fail("test_substring - 5");
		}catch( IndexOutOfBoundsException e ){}


		harness.check(!( !"hamburger".substring(4,8).equals("urge")), 
			"test_substring - 6");
		harness.check(!( !"smiles".substring(1,5).equals("mile")), 
			"test_substring - 7");
		harness.check(!( !"emptiness".substring(2,2).equals("")), 
			"test_substring - 8");

		try {
			String str = "hi there".substring(-1, 3);
			harness.fail("test_substring - 9");
		}catch( IndexOutOfBoundsException e ){}

		try {
			String str = "hi there".substring(0, 10);
			harness.fail("test_substring - 10");
		}catch( IndexOutOfBoundsException e ){}

		try {
			String str = "hi there".substring(7, 6);
			harness.fail("test_substring - 11");
		}catch( IndexOutOfBoundsException e ){}


	}

	public void test_concat( )
	{
		try {
			String str = "help".concat(null);
			harness.fail("test_concat - 1");
		}catch ( NullPointerException e){}

		harness.check(!( !"help".concat("me").equals("helpme")), 
			"test_concat - 2");

		harness.check(!( ! "to".concat("get").concat("her").equals("together")), 
			"test_concat - 3");

		harness.check(!( "hi".concat("") != "hi"), 
			"test_concat - 4");

		String str1 = "".concat("there");
		harness.check(!( !str1.equals("there")), 
			"test_concat - 5");

		// EJWcr00467
		String str2 = new String();
		try {
		    str2 = str2.concat("hello");
		    if (!str2.equals("hello")) {
			harness.fail("test_concat - 7");
		    }
		} catch (Exception e) {
			harness.fail("test_concat - 6");
		}
	}


	public void test_replace()
	{
		harness.check(!( !"mesquite in your cellar".replace('e' , 'o' ).equals(
			          "mosquito in your collar" )), 
			"test_replace - 1");

		harness.check(!( !"the war of baronets".replace('r' , 'y' ).equals(
			          "the way of bayonets" )), 
			"test_replace - 2");

		harness.check(!( !"sparring with a purple porpoise".replace('p' , 't' ).equals(
			          "starring with a turtle tortoise" )), 
			"test_replace - 3");

		harness.check(!( !"JonL".replace('q' , 'x' ).equals("JonL" )), 
			"test_replace - 4");

		harness.check(!( !"ppppppppppppp".replace('p' , 'p' ).equals("ppppppppppppp")), 
			"test_replace - 5");

		harness.check(!( !"ppppppppppppp".replace('p' , '1' ).equals("1111111111111")), 
			"test_replace - 6");
		harness.check(!( !"hp".replace('c' , 'd' ).equals("hp")), 
			"test_replace - 7");
		harness.check(!( !"vmhere".replace('a' , 'd' ).equals("vmhere")), 
			"test_replace - 8");


	}

	public void test_toLowerCase()
	{
		harness.check(!( !"".toLowerCase().equals("")), 
			"test_toLowerCase - 1");

		harness.check(!( !"French Fries".toLowerCase().equals("french fries")), 
			"test_toLowerCase - 2");


		harness.check(!( !"SMALL-VM".toLowerCase().equals("small-vm")), 
			"test_toLowerCase - 3");
	}

	public void test_toUpperCase()
	{
		harness.check(!( !"".toUpperCase().equals("")), 
			"test_toUpperCase - 1");

		harness.check(!( !"French Fries".toUpperCase().equals("FRENCH FRIES")), 
			"test_toUpperCase - 2");


		harness.check(!( !"SMALL-VM".toUpperCase().equals("SMALL-VM")), 
			"test_toUpperCase - 3");

		harness.check(!( !"small-jvm".toUpperCase().equals("SMALL-JVM")), 
			"test_toUpperCase - 4");
	}


	public void test_valueOf()
	{
		harness.check(!( !String.valueOf((Object)null).equals("null")), 
			"test_valueOf - 1");

		Object obj = new Object();
		harness.check(!( !String.valueOf(obj).equals(obj.toString())), 
			"test_valueOf - 2");


		try {
			char [] data = null;
			String str = String.valueOf( data );
		}catch ( NullPointerException e ){}

		char [] data = { 'h' , 'e' , 'l' , 'l' , 'o' };
		harness.check(!( !String.valueOf( data ).equals("hello")), 
			"test_valueOf - 3");

		harness.check(!( !String.copyValueOf( data ).equals("hello")), 
			"test_valueOf - 3a");

		try {
			String str = String.valueOf(data , -1 , 4 );
			harness.fail("test_valueOf - 4");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			String str = String.valueOf(data , 1 , 5 );
			harness.fail("test_valueOf - 5");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			String str = String.valueOf(data , 1 , -5 );
			harness.fail("test_valueOf - 6");
		}catch ( IndexOutOfBoundsException e ){}

		try {
			String str = String.valueOf(null , 1 , 3 );
			harness.fail("test_valueOf - 7");
		}catch ( NullPointerException e ){}

		harness.check(!( !String.valueOf(data , 2 , 2 ).equals("ll")), 
			"test_valueOf - 8");

		harness.check(!( !String.copyValueOf(data , 2 , 2 ).equals("ll")), 
			"test_valueOf - 8a");

		harness.check(!( !String.valueOf(true).equals("true")), 
			"test_valueOf - 9");

		harness.check(!( !String.valueOf(false).equals("false")), 
			"test_valueOf - 10");

		harness.check(!( !String.valueOf('c').equals("c")), 
			"test_valueOf - 11");

		harness.check(!( !String.valueOf(' ').equals(" ")), 
			"test_valueOf - 12");

		harness.check(!( !String.valueOf(234).equals("234")), 
			"test_valueOf - 13");

		harness.check(!( !String.valueOf(234L).equals("234")), 
			"test_valueOf - 14");

		harness.check(!( !String.valueOf(23.45f).equals("23.45")), 
			"test_valueOf - 15");

		harness.check(!( !String.valueOf(23.4).equals("23.4")), 
			"test_valueOf - 16");
	}
        
        public void test_intern()
	{
 	 	String hp = "hp";
		String nullstr = "";
		harness.check(!( "hp".intern() != hp.intern()), 
			"test_intern - 1");
		harness.check(!( "pqr".intern() == hp.intern()), 
			"test_intern - 2");
		harness.check(!( "".intern() != nullstr.intern()), 
			"test_intern - 3");
		harness.check(!( "".intern() == hp.intern()), 
			"test_intern - 4");
		hp = "";
		harness.check(!( "".intern() != hp.intern()), 
			"test_intern - 5");
		StringBuffer buff= new StringBuffer();
		buff.append('a');
		buff.append('b');
		harness.check(!( "ab".intern() != buff.toString().intern()), 
			"test_intern - 6");
		StringBuffer buff1 = new StringBuffer();
		harness.check(!( "".intern() != buff1.toString().intern()), 
			"test_intern - 7");

	}
	public void test_trim()
	{
	    String source = "   laura";
	    String dest;

	    dest = source.trim();
	    if (!dest.equals("laura")) {
		harness.fail("Error - test_trim - 1");
		System.out.println("expected 'laura', got '" + dest + "'");
	    }

	    source = "			laura";
	    dest = source.trim();
	    if (!dest.equals("laura")) {
		harness.fail("Error - test_trim - 2");
		System.out.println("expected 'laura', got '" + dest + "'");
	    }

	    source = "              ";
	    dest = source.trim();
	    if (!dest.equals("")) {
		harness.fail("Error - test_trim - 3");
		System.out.println("expected '', got '" + dest + "'");
	    }
	    source = "laura";
	    dest = source.trim();
	    if (dest != source) {
		harness.fail("Error - test_trim - 4");
		System.out.println("Expected strings to be equal");
	    }
	    source = "l        ";
	    dest = source.trim();
	    if (!dest.equals("l")) {
		harness.fail("Error - test_trim - 5");
		System.out.println("expected 'l', got '" + dest + "'");
	    }
	    source = "           l";
	    dest = source.trim();
	    if (!dest.equals("l")) {
		harness.fail("Error - test_trim - 6");
		System.out.println("expected 'l', got '" + dest + "'");
	    }
	    source = "           l            ";
	    dest = source.trim();
	    if (!dest.equals("l")) {
		harness.fail("Error - test_trim - 7");
		System.out.println("expected 'l', got '" + dest + "'");
	    }
	    source = "           l a u r a             ";
	    dest = source.trim();
	    if (!dest.equals("l a u r a")) {
		harness.fail("Error - test_trim - 8");
		System.out.println("expected 'l a u r a', got '" + dest + "'");
	    }
	}

	public void testall()
	{
		test_Basics();
		test_toString();
		test_equals();
		test_hashCode();
		test_length();
		test_charAt();
		test_getChars();
		test_getBytes();	
		test_toCharArray();
		test_equalsIgnoreCase();
		test_compareTo();
		test_regionMatches();
		test_startsWith();
		test_endsWith();
		test_indexOf();
		test_lastIndexOf();
		test_substring();
		test_concat();
		test_replace();
		test_toLowerCase();
		test_toUpperCase();
		test_valueOf();
		test_intern();
		test_trim();
	}


  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
