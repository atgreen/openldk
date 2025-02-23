// $Id: pr249.java,v 1.5 1999/11/04 14:59:45 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public interface EzlDict {
	public Object get(String key);
	public void put(String key, Object value);
	public Object call(String name, Object[] argv, Object argd);
	public Object call_new(Object[] argv, Object argd);
	public String  name;
	public EzlEnumerate  enumerate();
}
interface EzlEnumerate {
	boolean  next_elements(Object[] key_value);
}

