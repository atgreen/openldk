// $Id: pr301.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.awt.*;
import javax.swing.*;
import javax.swing.table.*;

class Main
{
	public final static void main(String argv[])
	{
		JTable table = new JTable(rowData,columnNames)
		{
			public final TableCellRenderer getCellRenderer(int row,int column)
					{ return renderer; }

			TableCellRenderer renderer=new DefaultTableCellRenderer()
			{
				public final Component getTableCellRendererComponent(JTable
					table,
					Object value,boolean isSelected,boolean hasFocus,int row,int
					column)
				{
					Component c=super.getTableCellRendererComponent(table,value,
						isSelected,hasFocus,row,column);
					if (column%2==0)
						c.setBackground(Color.yellow);
					else
						c.setBackground(Color.gray);
					return c;
				}
			};
		};
		JScrollPane scrollpane = new JScrollPane(table);
		JFrame frame=new JFrame("test");
		frame.setSize(400,300);
		frame.getContentPane().add(scrollpane);
		frame.show();
	}

	static final Object rowData[][]=new Object[][]
									{{"1","2","3"},{"1","2","3"},{"1","2","3"}};

	static final Object columnNames[]=new Object[]
									  {"A","B","C"};
}

