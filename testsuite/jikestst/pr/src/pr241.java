// $Id: pr241.java,v 1.5 1999/11/04 14:59:45 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class Test extends Frame
{
	...
			class InnerClass implements ActionListener
	{
				public void actionPerformed(ActionEvent evt)
				{
					Dialog dialog = new TestDialog(this$0);
					dialog.setVisible(true);
				}
	}

}


