// $Id: pr339.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// Create the menus for the interface.
private MenuBar buildMenus() {
	MenuBar menuBar = new MenuBar();

		// Creating the "Server" menu
	Menu serverMenu = new Menu("Server");

	MenuItem monitorServerItem = new MenuItem("Monitor a remote
			server");
	monitorServerItem.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			new CreateServerMonitorUI(myServer);
		}
	});
	serverMenu.add(monitorServerItem);
}
