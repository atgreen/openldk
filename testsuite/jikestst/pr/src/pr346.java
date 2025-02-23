// $Id: pr346.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

/* Source to generate the error */

import java.awt.*;
import com.sun.java.swing.*;


class CrashTest extends JFrame
{
	public static void main(String arg[])
	{
		new CrashTest().run();
	}

	public void run () {

		class MyGridBagLayout extends java.awt.GridBagLayout {
			protected GridBagConstraints defaultConstraints;

			public MyGridBagLayout() {
				super();
				GridBagConstraints c = new GridBagConstraints();
				defaultConstraints = c;
			}
		};

		GridBagLayout gridBag = new MyGridBagLayout();
	}
}
