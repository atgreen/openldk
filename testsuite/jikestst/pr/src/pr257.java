// $Id: pr257.java,v 1.5 1999/11/04 14:59:45 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class JikesExceptions
{
	public static void main(String args[])
	{
		new JikesExceptions();
	}


	private JikesExceptions()
	{
		int test[] = {1,2,3,4,5};

		int a=0;
		try
		{
			while(true)
			{
				int x = test[a++];
				System.out.println(x);
			}
		}
		catch (Exception e)
		{
			// Catch the exception thrown from the array gets blown.
		}

		System.out.println("0");
	}
}
