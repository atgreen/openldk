// $Id: pr371d.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

package com.qwest.common.account;

public class AccountFactory
{
	private static AccountFactory c_AccountFactory=new AccountFactory();
	private AccountFactory()
	{
	}
	public static AccountFactory getInstance()
	{
		return c_AccountFactory;
	}

	public Account createAccount()
	{
		return new AccountImpl();
	}
}

