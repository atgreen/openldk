// $Id: pr320.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


import java.awt.*;
import java.awt.event.*;
import com.sun.java.swing.*;

import java.util.*;

/**
 * MainWindow.java
 *
 *
 * Created: Thu Oct 22 17:31:12 1998
 *
 * @author Sebastian Fischmeister
 * @version
 */

public class MainWindow extends JFrame {

	private JMenuBar menubar;
	private JToolBar toolbar;
	private OutputLayer outputlayer;
	JTextField counter;

	public MainWindow() {
		super("Classification via Clustering");

		Globals.initializeGlobals();

		setSize(500, 500);

		menubar = new JMenuBar();
		JMenu fileMenu = new JMenu("File");
		JMenu optionMenu = new JMenu("Options");
		JMenu helpMenu = new JMenu("Help");

	/*
     *  Hauptmenü
     * 
     */
		JMenuItem resetMenuItem = new JMenuItem("Reset");
		JMenuItem quitMenuItem = new JMenuItem("Exit");
		JMenuItem optionsMenuItem = new JMenuItem("Options");

		fileMenu.add(resetMenuItem);
		fileMenu.add(optionsMenuItem);
		fileMenu.addSeparator();
		fileMenu.add(quitMenuItem);

	/*
     * Optionmenu
     */

		JCheckBoxMenuItem winnerMenuItem = new JCheckBoxMenuItem ("showWinner");
		winnerMenuItem.setState(true);
		Globals.getGlobals().getProperties().put("showwinner","1");

		winnerMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.out.println("event: "+e.getActionCommand());

				Hashtable ht = Globals.getGlobals().getProperties();
				if(ht.get("showwinner").equals("1"))
					ht.put("showwinner","0");
				else
					ht.put("showwinner","1");

			}});


			JCheckBoxMenuItem neighbourMenuItem = new JCheckBoxMenuItem ("showNeighbours");
			Globals.getGlobals().getProperties().put("showneighbours","0");
			neighbourMenuItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					System.out.println("event: "+e.getActionCommand());

					Hashtable ht = Globals.getGlobals().getProperties();
					if(ht.get("showneighbours").equals("1"))
						ht.put("showneighbours","0");
					else
						ht.put("showneighbours","1");

				}});


				optionMenu.add(winnerMenuItem);
				optionMenu.add(neighbourMenuItem);


	/*
     * Hilfemenü
     * 
     */

				JMenuItem aboutMenuItem = new JMenuItem("About");

				resetMenuItem.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {

						int anz = Integer.parseInt((String)Globals.getGlobals().getProperties().get("size"));
						Globals.getGlobals().setRuns(0);
						Globals.getGlobals().initializeNeuronLayer(anz);
						MainWindow.this.getContentPane().repaint();

					}});

					quitMenuItem.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							System.out.println("Hopefully, you had fun with this program !");
							System.exit(0);
						}});

						aboutMenuItem.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent e) {
								JOptionPane.showMessageDialog(null, 
									getTitle()+"\n$Revision: 1.2 $\n(c) Sebastian Fischmeister 1998", 
									"About",
									JOptionPane.INFORMATION_MESSAGE);
							}});

							optionsMenuItem.addActionListener(new ActionListener() {

								ConfigureDialog cd;

								public void actionPerformed(ActionEvent e) {
									System.out.println("opening configure-dialog");

									if(cd == null) {
										cd = new ConfigureDialog();
										new ConfigureDialogToMainWindowAdapter(cd,MainWindow.this);
									}
									cd.show();
								}});


								helpMenu.add(aboutMenuItem);

								menubar.add(fileMenu);
								menubar.add(optionMenu);
								menubar.add(helpMenu);
								setJMenuBar(menubar);

	/**

       ToolBar

    **/
								this.getContentPane().setLayout(new GridBagLayout());

								JButton but = new JButton("Step");
								but.addActionListener(new ActionListener() {
									public void actionPerformed(ActionEvent e) {

										MainWindow.this
												.getAccessibleContext()
												.getAccessibleComponent()
												.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

										Trainer trainer = Globals.getGlobals().getTrainer();
										trainer.train();
										Globals.getGlobals().setRuns(Globals.getGlobals().getRuns()+1);
										MainWindow.this.counter.setText(""+Globals.getGlobals().getRuns());

										MainWindow.this.outputlayer.repaint();

										MainWindow.this
												.getAccessibleContext()
												.getAccessibleComponent()
												.setCursor(Cursor.getDefaultCursor());

									}});

									add(this.getContentPane(), but,
										0,0, 1, 1,
										GridBagConstraints.HORIZONTAL, GridBagConstraints.NORTH, 0, 0,
										1, 0.2,
										5, 5, 5, 5 );

									but = new JButton("Next");
									but.addActionListener(new ActionListener() {
										public void actionPerformed(ActionEvent e) {
											MainWindow.this
													.getAccessibleContext()
													.getAccessibleComponent()
													.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

											Trainer trainer = Globals.getGlobals().getTrainer();
											int anz = Integer.parseInt((String)Globals.getGlobals().getProperties().get("iterations"));
											Globals.getGlobals().setRuns(Globals.getGlobals().getRuns()+anz);
											trainer.train(anz);
											MainWindow.this.counter.setText(""+(Globals.getGlobals().getRuns()));

											MainWindow.this.outputlayer.repaint();
											MainWindow.this
													.getAccessibleContext()
													.getAccessibleComponent()
													.setCursor(Cursor.getDefaultCursor());
										}});

										add(this.getContentPane(), but,
											-1,-1 , 1, 1,
											GridBagConstraints.HORIZONTAL, GridBagConstraints.NORTH, 0, 0,
											1, 0.2,
											5, 5, 5, 5 );


										but = new JButton("Test");
										but.addActionListener(new ActionListener() {
											TestDialog td = null;
											public void actionPerformed(ActionEvent e) {

												if(td == null) {
													td = new TestDialog();
													new TestDialogToMainWindowAdapter(MainWindow.this,
														td);
												}
												td.show();
											}});

											add(this.getContentPane(), but,
												-1,-1 , 1, 1,
												GridBagConstraints.HORIZONTAL, GridBagConstraints.NORTH, 0, 0,
												1, 0.2,
												5, 5, 5, 5 );

											counter = new JTextField("0");
											counter.setEditable(false);
											add(this.getContentPane(), counter,
												-1,-1 , GridBagConstraints.REMAINDER, 1,
												GridBagConstraints.HORIZONTAL, GridBagConstraints.NORTH, 0, 0,
												1, 0.2,
												5, 5, 5, 5 );


	/**

       OutputLayer

    **/

											outputlayer = new OutputLayer();
											this.addWindowListener(new WindowAdapter() {
												public void windowClosing(WindowEvent e) {System.exit(0);}
												public void windowOpened(WindowEvent e) {}});


												add(this.getContentPane(), outputlayer,
													-1,-1 , GridBagConstraints.REMAINDER, GridBagConstraints.REMAINDER,
													GridBagConstraints.BOTH, GridBagConstraints.NORTH, 0, 0,
													1, 1,
													10, 10, 10, 10 );


	//     getContentPane().setLayout(new BorderLayout());    
	//     getContentPane().add("North", toolbar);
	//     getContentPane().add("Center",outputlayer);
												reshape(100, 100, 500, 500);
												validate();
	}

	private JButton createToolbarButton(String key,String icon, ActionListener listener) {
		JButton b = new JButton(new ImageIcon(icon)) {
			public float getAlignmentY() { return 0.5f; }
		};

		b.setRequestFocusEnabled(false);
		b.setMargin(new Insets(1,1,1,1));

		b.addActionListener(listener);
		b.setToolTipText(key);

		return b;
	}

	protected void add( Container container, Component component,
						int gridx, int gridy, int gridw, int gridh,
						int fill, int anchor, int ipadx, int ipady,
						double wgtx, double wgty,
						int top, int left, int right, int bottom ) {
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = gridx; c.gridy = gridy;
		c.gridwidth = gridw; c.gridheight = gridh;
		c.fill = fill; c.anchor = anchor;
		c.ipadx = ipadx; c.ipady = ipady;
		c.weightx = wgtx; c.weighty = wgty;
		c.insets = new Insets(top, left, bottom, right);
		((GridBagLayout)(container.getLayout())).setConstraints(component, c);
		container.add(component);
	}



	public static void main(String[] args) {

		MainWindow f = new MainWindow();
		f.show();

	}

} // MainWindow

