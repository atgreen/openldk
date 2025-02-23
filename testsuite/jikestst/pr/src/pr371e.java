// $Id: pr371e.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

package com.qwest.common.account.test;

import com.qwest.common.account.AccountFactory;
import com.qwest.common.account.Account;

public class TestAccount
{
	public static void main(String args[])
	{
		AccountFactory m_Factory = AccountFactory.getInstance();
		Account m_Account=m_Factory.createAccount();
		System.out.println(m_Account.getAttribute1());
//    long l_blah=m_Account.getIdentity();
	//System.out.println(m_Account.getIdentity());
		System.out.println(m_Account.getCustomer());
	}
}
