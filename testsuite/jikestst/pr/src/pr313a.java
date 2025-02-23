// $Id: pr313a.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.lang.*;

class GConstants {
	public static class GDisplay {
		public  class ContainerType {
			final String PANEL = "panel";
			String type;
			ContainerType(String str){
				if (str.equals(PANEL))
					type = PANEL;
			}
			boolean equals(ContainerType c){ return c.type == type; }
		}
	}
}
