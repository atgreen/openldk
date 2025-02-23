// $Id: pr313b.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.lang.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.tree.*;

import GConstants;
class GDisplay extends JPanel {
	GConstants.GDisplay.ContainerType  containerType;

	GConstants.GDisplay.ContainerType getContainerType(){
		return containerType;
	}
	void setContainerType(GConstants.GDisplay.ContainerType type){
		containerType = type;
	}
}
