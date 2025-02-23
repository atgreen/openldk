// $Id: pr128.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.net.*;
import java.util.*;

final class GaspodeParameters {
	private static final URL[] uriList;

	static {
		try {

			uriList = new URL[] {new URL("http", "127.0.0.1", 8080, ""),
			new URL("http", "127.0.0.1", 8081, "")};

		} catch (MalformedURLException ignore) {

			uriList = null;
		}
	}
 public static void main(String[] args) {
  System.out.println("0");
 }
}

