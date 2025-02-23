// $Id: pr400.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class ifZuordnen extends JInternalFrame implements ActionListener
{
	String test[] = new String[tm.getRowCount()];  // THIS WILL NOT WORK
	public TableModel tm = new AbstractTableModel() {
		private static final int rowCount = 15;
		public int getRowCount() { return rowCount; }
		...
