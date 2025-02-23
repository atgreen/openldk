// $Id: pr196.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Button;
import java.awt.Panel;
import java.awt.Frame;

public class Test extends Frame implements ActionListener {

	Button ok;
	Panel panel;

	public Test() {
		panel = new Panel();
		ok = new Button("Ok");
		panel.add(ok);
		ok.addActionListener(this);
	}

	public void actionPerformed(ActionEvent evt) {
		System.out.println("Button clicked");

	}
 public static void main(String[] args) {
  System.out.println("0");
 }



}

