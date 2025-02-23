// $Id: pr277.java,v 1.5 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// Works fine with JDK 1.1.7
// Won't compile with jikes
public class Jikes
   implements Jikes.Constants
{
	   public interface Constants
	   {
		   public final static String NAME = "Mike Hinchey";
	   }
	   public Jikes()
	   {
		   System.out.println(NAME);
	   }
	   public static void main( String args[] )
	   {
		   new Jikes();
	   }
}

