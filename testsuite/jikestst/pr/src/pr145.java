// $Id: pr145.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.awt.image.*;

public class Bug extends ColorModel
{
	public int getRed(int i)
	{
		switch (i) {
			default:
			case 0: return 0xFF;
			case 1: return 0xC6;
			case 2: return 0x84;
			case 3: return 0x00;
			case 4: return 0x00;
		}
	}
	public int getGreen(int i)
	{
		switch (i) {
			default:
			case 0: return 0xFF;
			case 1: return 0xC3;
			case 2: return 0x82;
			case 3: return 0x00;
			case 4: return 0x00;
		}
	}
	public int getBlue(int i)
	{
		switch (i) {
			default:
			case 0: return 0xFF;
			case 1: return 0xC6;
			case 2: return 0x84;
			case 3: return 0x00;
			case 4: return 0xFF;
		}
	}
	public int getAlpha(int i)
	{
		return 0xFF;
	}
}

