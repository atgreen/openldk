// $Id: pr201a.java,v 1.5 1999/11/04 14:59:45 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class MainExceptions
{
	public static void handleException (Exception e) {
  //bunch of exception handling code, checks if e is
  //instanceof SQLException etc. E.g.:
		if (e instanceof InvalidCodeException)
		{
			System.err.println(e);
			msg.append(e.getMessage()).append("\n");
		}
  //finally throws up a dialog telling user about
  //about the exception
	}

}//end of public class

//Exceptions classes...(I have 6 of these)
class InvalidCodeException extends Exception {
	public InvalidCodeException (String msg){
		super(msg);
	}
}
