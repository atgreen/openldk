//Uses: CharInfo

/* Copyright (C) 1999 Artur Biesiadowski
   Copyright (C) 2004 Stephen Crawley
   Copyright (C) 2007 Joshua Sumali

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

package gnu.testlet.java.lang.Character;

import java.io.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.ResourceNotFoundException;

public abstract class UnicodeBase implements Testlet {

	public static boolean testDeprecated;
	public static boolean verbose;
	public static boolean benchmark;

	public int failures;
	public int tests;
	TestHarness harness;

	private Reader bir;
	private StringBuffer sb;

	public UnicodeBase()
	{
	}

	public UnicodeBase(TestHarness aHarness, String filename) 
	throws ResourceNotFoundException, FileNotFoundException
	{
		harness = aHarness;
		bir = harness.getResourceReader("gnu#testlet#java#lang#Character#" +
				filename);
	}

	private String getNext(Reader r) throws IOException
	{
		sb = new StringBuffer();
		while (r.ready())
		{
			char ch = (char) r.read();
			if (ch == '\r')
			{
				continue;
			}
			else if (ch == ';' ||  ch == '\n')
			{
				return sb.toString();
			}
			else
				sb.append(ch);
		}
		return sb.toString();
	}

	public void performTests() throws IOException{

		//actual test loop
		CharInfo ci = new CharInfo();
		while (bir.ready())
		{
			String str;
			ci = new CharInfo();
			// 0 - Code value
			str = getNext(bir);
			int code = Integer.parseInt(str, 16);
			ci.code = code;

			// 1 - Character name
			ci.name = getNext(bir);
			// 2 - General category
			ci.category = getNext(bir);
			// 3 - Canonical combining classes
			getNext(bir);
			// 4 - Bidirectional category
			getNext(bir);
			// 5 - Character decomposition mapping
			getNext(bir);
			// 6 - Decimal digit value
			str = getNext(bir);
			if (!str.equals(""))
				ci.decimalDigit = Integer.parseInt(str, 10);
			else
				ci.decimalDigit = -1;
			// 7 - Digit value
			str = getNext(bir);
			if (!str.equals(""))
				ci.digit = Integer.parseInt(str, 10);
			else
				ci.digit = -1;

			// 8 - Numeric value
			str = getNext(bir);
			if (str.equals(""))
			{
				ci.numericValue = -1;
			}
			else
			{
				try {
					ci.numericValue = Integer.parseInt(str, 10);
					if (ci.numericValue < 0)
						ci.numericValue = -2;
				} 
				catch (NumberFormatException e)
				{
					ci.numericValue = -2;
				}
			}
			// 9 - Mirrored
			getNext(bir);
			// 10 - Unicode 1.0 name
			getNext(bir);
			// 11 - ISO 10646 comment field
			getNext(bir);
			// 12 - Upper case mapping
			str = getNext(bir);
			if (!str.equals(""))
				ci.uppercase = Integer.parseInt(str, 16);
			else 
				ci.uppercase = ci.code;
			// 13 - Lower case mapping
			str = getNext(bir);
			if (!str.equals(""))
				ci.lowercase = Integer.parseInt(str, 16);
			else 
				ci.lowercase = ci.code;
			// 14 - Title case mapping
			str = getNext(bir);
			if (!str.equals(""))
				ci.titlecase = Integer.parseInt(str, 16);
			else 
				ci.titlecase = ci.code;

			// Character.digit() only treats "Nd" as decimal digits, not "No" 
			// or "Nl".  Tweak the character defns accordingly.
			if (ci.digit != -1 && !("Nd".equals(ci.category))) 
				ci.digit = -1;

			//test the char
			testChar(ci);
		}


		// Fill in the character ranges that are reserved in Unicode 3.0
		CharInfo ch = new CharInfo();
		ch.name = "CJK Ideograph";
		ch.category = "Lo";
		ch.decimalDigit = -1;
		ch.digit = -1;
		ch.numericValue = -1;
		for (int i = 0x4E01; i <= 0x9FA4; i++)
		{
			ch.code = i;
			testChar(ch);
		}

		ch = new CharInfo();
		ch.name = "CJK Ideograph Extension A";
		ch.category = "Lo";
		ch.decimalDigit = -1;
		ch.digit = -1;
		ch.numericValue = -1;
		for (int i = 0x3400; i <= 0x4DB5; i++)
		{
			ch.code = i;
			testChar(ch);
		}

		ch = new CharInfo();
		ch.name = "Hangul Syllable";
		ch.category = "Lo";
		ch.decimalDigit = -1;
		ch.digit = -1;
		ch.numericValue = -1;
		for (int i = 0xAC01; i <= 0xD7A2; i++)
		{
			ch.code = i;
			testChar(ch);
		}

		ch = new CharInfo();
		ch.name = "CJK Compatibility Ideograph";
		ch.category = "Lo";
		ch.decimalDigit = -1;
		ch.digit = -1;
		ch.numericValue = -1;
		for (int i = 0xF901; i <= 0xFA2C; i++)
		{
			ch.code = i;
			testChar(ch);
		}

		ch = new CharInfo();
		ch.name = "Surrogate";
		ch.category= "Cs";
		ch.decimalDigit = -1;
		ch.digit = -1;
		ch.numericValue = -1;
		for (int i = 0xD800; i <= 0xDFFFl; i++)
		{
			ch.code = i;
			testChar(ch);
		}

		ch = new CharInfo();
		ch.name = "Private Use";
		ch.category = "Co";
		ch.decimalDigit = -1;
		ch.digit = -1;
		ch.numericValue = -1;
		for (int i = 0xE000; i <= 0xF8FF; i++)
		{
			ch.code = i;
			testChar(ch);
		}
	}

	private void testChar(CharInfo c){

		//All the checkPassed() calls are commented out since if they're
		//included, this creates too many getStackTrace() calls in
		//RunnerProcess.java, resulting in the heap running out of memory.
		
		// isLowerCase
		//char i = (char) x;
		if ("Ll".equals(c.category) != Character.isLowerCase( c.code))
		{
			reportError(c,
					(Character.isLowerCase(c.code) ? "lowercase" :
					"not-lowercase"));

		}
		//else checkPassed();

		// isUpperCase
		if ("Lu".equals(c.category) != Character.isUpperCase(c.code))
		{
			reportError(c,
					(Character.isUpperCase((char) c.code) ? "uppercase" :
					"not-uppercase"));
		}
		//else checkPassed();

		// isTitleCase
		if ( "Lt".equals(c.category) !=
			Character.isTitleCase(c.code))
		{
			reportError(c,
					(Character.isTitleCase((char) c.code) ? "titlecase" :
					"not-titlecase"));
		}
		//else checkPassed();

		// isDigit
		if ("Nd".equals(c.category) != Character.isDigit(c.code))
		{
			reportError(c,
					(Character.isDigit((char) c.code) ? "digit" : "not-digit"));
		}
		//else checkPassed();

		// isDefined
		if (!c.category.equals("Cn") != Character.isDefined(c.code))
		{
			reportError(c,
					(Character.isDefined((char) c.code) ? "defined" : 
					"not-defined"));
		}
		//else checkPassed();

		// isLetter
		if ((c.category.charAt(0) == 'L') != 
			Character.isLetter(c.code))
		{
			reportError(c,
					(Character.isLetter((char) c.code) ? "letter" : 
					"not-letter"));
		}
		//else checkPassed();

		// isLetterOrDigit
		if (Character.isLetterOrDigit(c.code) !=
			(Character.isLetter(c.code) || Character.isDigit(c.code)))
		{
			reportError(c,
					(Character.isLetterOrDigit(c.code) ? "letterordigit" :
					"not-letterordigit"));
		}
		//else checkPassed();

		// isSpaceChar
		if ((c.category.charAt(0) == 'Z') != Character.isSpaceChar(c.code))
		{
			reportError(c,
					(Character.isSpaceChar(c.code) ? "spacechar" : 
					"not-spacechar"));
		}
		//else checkPassed();

		// isWhiteSpace
		if (whitespace(c) != Character.isWhitespace(c.code))
		{
			reportError(c,
					Character.isWhitespace(c.code) ? "whitespace" : 
			"not-whitespace");
		}
		//else checkPassed();

		// isISOControl
		if (((c.code <= 0x001F) || range(c.code, 0x007F, 0x009F)) !=
			Character.isISOControl(c.code))
		{
			reportError(c,
					Character.isISOControl(c.code) ? "isocontrol" :
			"not-isocontrol");
		}
		//else checkPassed();

		int type = Character.getType(c.code);
		String typeStr = null;
		switch (type)
		{
		case Character.UNASSIGNED: typeStr = "Cn"; break;
		case Character.UPPERCASE_LETTER: typeStr = "Lu"; break;
		case Character.LOWERCASE_LETTER: typeStr = "Ll"; break;
		case Character.TITLECASE_LETTER: typeStr = "Lt"; break;
		case Character.MODIFIER_LETTER: typeStr = "Lm"; break;
		case Character.OTHER_LETTER: typeStr = "Lo"; break;
		case Character.NON_SPACING_MARK: typeStr = "Mn"; break;
		case Character.ENCLOSING_MARK: typeStr = "Me"; break;
		case Character.COMBINING_SPACING_MARK: typeStr = "Mc"; break;
		case Character.DECIMAL_DIGIT_NUMBER: typeStr = "Nd"; break;
		case Character.LETTER_NUMBER: typeStr = "Nl"; break;
		case Character.OTHER_NUMBER: typeStr = "No"; break;
		case Character.SPACE_SEPARATOR: typeStr = "Zs"; break;
		case Character.LINE_SEPARATOR: typeStr = "Zl"; break;
		case Character.PARAGRAPH_SEPARATOR: typeStr = "Zp"; break;
		case Character.CONTROL: typeStr = "Cc"; break;
		case Character.FORMAT: typeStr = "Cf"; break;
		case Character.PRIVATE_USE: typeStr = "Co"; break;
		case Character.SURROGATE: typeStr = "Cs"; break;
		case Character.DASH_PUNCTUATION: typeStr = "Pd"; break;
		case Character.START_PUNCTUATION: typeStr = "Ps"; break;
		case Character.END_PUNCTUATION: typeStr = "Pe"; break;
		case Character.CONNECTOR_PUNCTUATION: typeStr = "Pc"; break;
		case Character.FINAL_QUOTE_PUNCTUATION: typeStr = "Pf"; break;
		case Character.INITIAL_QUOTE_PUNCTUATION: typeStr = "Pi"; break;
		case Character.OTHER_PUNCTUATION: typeStr = "Po"; break;
		case Character.MATH_SYMBOL: typeStr = "Sm"; break;
		case Character.CURRENCY_SYMBOL: typeStr = "Sc"; break;
		case Character.MODIFIER_SYMBOL: typeStr = "Sk"; break;
		case Character.OTHER_SYMBOL: typeStr = "So"; break;
		default: typeStr = "ERROR (" + type + ")"; break;
		}

		if (!(c.category.equals(typeStr) ||
				(typeStr.equals("Ps") && c.category.equals("Pi")) ||
				(typeStr.equals("Pe") && c.category.equals("Pf"))))
		{
			reportError(stringChar(c) + " is reported to be type " + typeStr +
					" instead of " + c.category);
		}
		//else checkPassed();

		// isJavaIdentifierStart
		if (identifierStart(c) != Character.isJavaIdentifierStart(c.code))
		{
			reportError(c,
					Character.isJavaIdentifierStart(c.code) ?
							"javaindentifierstart" : "not-javaidentifierstart");
		}
		//else checkPassed();

		// isJavaIdentifierPart
		typeStr = c.category;
		if ((typeStr.charAt(0) == 'L' ||
				typeStr.equals("Sc") ||
				typeStr.equals("Pc") ||
				typeStr.equals("Nd") ||
				typeStr.equals("Nl") ||
				typeStr.equals("Mc") ||
				typeStr.equals("Mn") ||
				typeStr.equals("Cf") ||
				(typeStr.equals("Cc") && ignorable(c))) != 
					Character.isJavaIdentifierPart(c.code))
		{
			reportError(c,
					Character.isJavaIdentifierPart(c.code) ? 
							"javaidentifierpart" : "not-javaidentifierpart");
		}
		//else checkPassed();

		//isUnicodeIdentifierStart
		if (unicodeIdentifierStart(c) != Character.isUnicodeIdentifierStart(c.code))
		{
			reportError(c,
					Character.isUnicodeIdentifierStart(c.code) ? 
							"unicodeidentifierstart" : 
			"not-unicodeidentifierstart");
		}
		//else checkPassed();

		//isUnicodeIdentifierPart;
		typeStr = c.category;
		if ((typeStr.charAt(0) == 'L' ||
				typeStr.equals("Pc") ||
				typeStr.equals("Nd") ||
				typeStr.equals("Nl") ||
				typeStr.equals("Mc") ||
				typeStr.equals("Mn") ||
				typeStr.equals("Cf") ||
				(typeStr.equals("Cc") && ignorable(c))) != 
					Character.isUnicodeIdentifierPart(c.code))
		{
			reportError(c,
					Character.isUnicodeIdentifierPart(c.code) ?
							"unicodeidentifierpart" : "not-unicodeidentifierpart");
		}
		//else checkPassed();


		//isIdentifierIgnorable
		if (ignorable(c) != Character.isIdentifierIgnorable(c.code))
		{
			reportError(c,
					Character.isIdentifierIgnorable(c.code) ? 
							"identifierignorable": "not-identifierignorable");
		}
		//else checkPassed();


		// toLowerCase
		int lowerCase = (c.lowercase != 0 ?
				c.lowercase : c.code);
		if (Character.toLowerCase(c.code) != lowerCase)
		{
			reportError(stringChar(c) + " has wrong lowercase form of " +
					c.lowercase +" instead of " +
					stringChar(c));
		}
		//else checkPassed();

		// toUpperCase

		int upperCase = (c.uppercase != 0 ?
				c.uppercase : c.code);
		if (Character.toUpperCase(c.code) != upperCase)
		{
			reportError(stringChar(c) +
					" has wrong uppercase form of " +
					c.uppercase +
					" instead of " +
					stringChar(c));
		}
		//else checkPassed();

		// toTitleCase
		int titleCase = (c.titlecase != 0 ? c.titlecase :
			(c.uppercase != 0 ? 
					c.uppercase : c.code));
		if ("Lt".equals(c.category))
		{
			titleCase = c.code;
		}

		if (Character.toTitleCase(c.code) != titleCase)
		{
			reportError(stringChar(c) +
					" has wrong titlecase form of " +
					c.titlecase +
					" instead of " + 
					stringChar(c));
		}
		//else checkPassed();

		// digit
		boolean radixPassed = true;
		for (int radix = Character.MIN_RADIX; radix <= Character.MAX_RADIX;
		radix++)
		{
			//special cases for A-Za-z and their fullwidth counterparts
			if (range(c.code,'A','Z')){
				c.digit = c.code - 'A' + 10;
			} 
			else if (range(c.code,'a','z')){
				c.digit = c.code - 'a' + 10;
			} 
			else if (range(c.code,0xff21,0xff3a)){
				c.digit = c.code - 0xff21 + 10;
			} 
			else if (range(c.code,0xff41,0xff5a)){
				c.digit = c.code - 0xff41 + 10;
			}
			int digit = c.digit;
			if (digit >= radix) 
				digit = -1;
			if (Character.digit( c.code, radix) != digit)
			{
				reportError(stringChar(c) + " has wrong digit form of " +
						Character.digit(c.code, radix) + " for radix " + 
						radix + " instead of " + digit +
						"(" + c.digit + ")");
				radixPassed = false;
			}
			//else checkPassed();
		}
		if (radixPassed)
			checkPassed();


		// getNumericValue

		if (range(c.code,'A','Z') || range(c.code,'a','z')
				|| range(c.code,0xff21,0xff3a) || range(c.code,0xff41,0xff5a)){
			if(c.numericValue != -1){
				reportError(stringChar(c) + " has wrong numeric value of " +
						Character.getNumericValue(c.code) + " instead of -1");
			}
		} else {

			if (c.numericValue != Character.getNumericValue(c.code))
			{
				reportError(stringChar(c) + " has wrong numeric value of " +
						Character.getNumericValue(c.code) + " instead of " + 
						c.numericValue);
			}

		}
		if (testDeprecated)
		{

			// isJavaLetter
			if (((char) c.code == '$' || (char) c.code == '_' 
				|| Character.isLetter(c.code)) !=	Character.isJavaLetter((char) c.code))
			{
				reportError(c,
						(Character.isJavaLetter((char) c.code)? "javaletter" : 
						"not-javaletter"));
			}
			//else checkPassed();

			// isJavaLetterOrDigit
			if ((Character.isJavaLetter((char) c.code) || Character.isDigit(c.code) ||
					(char) c.code == '$' || (char) c.code == '_') !=
						Character.isJavaLetterOrDigit((char) c.code)
			)
			{
				reportError(c,
						(Character.isJavaLetterOrDigit((char) c.code) ?
								"javaletterordigit" : "not-javaletterordigit"));
			}
			//else checkPassed();

			// isSpace
			if ((((char) c.code == ' ' || (char) c.code == '\t' 
				|| (char) c.code == '\n' || (char) c.code == '\r' ||
				(char) c.code == '\f')) != Character.isSpace((char) c.code))
			{
				reportError(c,
						(Character.isSpace((char) c.code) ? "space" : "non-space"));
			}
			//else checkPassed();
		} // testDeprecated


	}

	protected void reportError(CharInfo c, String what)
	{
		harness.check(false, stringChar(c) +" incorrectly reported as " + what);
	}
	protected void reportError( String what)
	{
		harness.check(false, what);
	}
	protected void checkPassed()
	{
		harness.check(true);
	}

	public boolean range(int mid, int low, int high)
	{
		return (mid >= low && mid <= high);
	}

	public boolean whitespace(CharInfo c) 
	{
		return ((c.category.charAt(0) == 'Z' && 
				c.code != 0x00a0 && c.code != 0x2007 && c.code != 0x202f) ||
				range(c.code, 0x0009, 0x000D) || 
				range(c.code, 0x001C, 0x001F));
	}

	//public String stringChar(int ch)
	public String stringChar(CharInfo c)
	{
		//return "Character " + Integer.toString(c.code,16) + ":"
		return "Character " + c.code + ":"
		+ (char) c.code + ":" + c.name;
	}

	public boolean identifierStart(CharInfo c) 
	{
		return ("Ll".equals(c.category) || 
				"Lu".equals(c.category) || 
				"Lt".equals(c.category) || 
				"Lm".equals(c.category) || 
				"Lo".equals(c.category) || 
				"Nl".equals(c.category) || 
				"Sc".equals(c.category) || 
				"Pc".equals(c.category));
	}

	public boolean unicodeIdentifierStart(CharInfo c) 
	{
		return ("Ll".equals(c.category) || 
				"Lu".equals(c.category) || 
				"Lt".equals(c.category) || 
				"Lm".equals(c.category) || 

				"Lo".equals(c.category) || 
				"Nl".equals(c.category));
	}

	public boolean ignorable(CharInfo c)
	{
		return (range(c.code, 0x0000, 0x0008) ||
				range(c.code, 0x000E, 0x001B) ||
				range(c.code, 0x007f, 0x009f) ||
				"Cf".equals(c.category));
	}
}
