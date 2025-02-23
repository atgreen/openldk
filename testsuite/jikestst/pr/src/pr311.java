// $Id: pr311.java,v 1.3 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

//   package org.codenest.reuse.viewer;

/**
 * Interface for a class which vends Components set to a given value.
 */
public interface ViewFactory extends ComponentFactory {

  /**
   * Creates a new view with the given value
   */
	public Component createComponent(Object value);

  /**
   * updates the given component (which must previously have been
   * created by this factory) with the given value
   */
	public void updateComponent(Component component, Object value);
}
