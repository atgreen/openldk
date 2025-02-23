// $Id: pr198.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.awt.event.*;
import java.awt.*;

public class JTest {

	Button b;

	private ActionListener listener = new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
			b.removeActionListener(ActionListener.this);
		}
	};


	public JTest() {
		b = new Button();
		b.addActionListener(listener);
	}

}

