// $Id: pr290.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// pr 290 -- uses swing
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.plaf.basic.*;

public class MyTableHeaderUI extends BasicTableHeaderUI {
	public class MyMouseInputHandler extends BasicTableHeaderUI.MouseInputHandler {
		public void mousePressed (MouseEvent e) {
			if (SwingUtilities.isLeftMouseButton (e)) {
				super.mousePressed (e);
			}
		}
	}

	// FIXME:  Change to BasicTableHeaderUI.MouseInputListener for big error
	// in Jikes
	protected BasicTableHeaderUI.MouseInputListener createMouseInputListener() {
		return new MouseInputHandler();
	}
}

