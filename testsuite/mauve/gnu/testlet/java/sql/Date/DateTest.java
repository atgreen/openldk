/*************************************************************************
/* DateTest.java - Test java.sql.Date
/*
/* Copyright (c) 2003 Dalibor Topic (robilad@yahoo.de)
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.2

package gnu.testlet.java.sql.Date;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.sql.*;

public class DateTest implements Testlet
{

public void
test(TestHarness harness)
{
    Date d = new Date(0);

    try {
      d.getHours();
      harness.check(false, "getHours");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "getHours");
    }

    try {
      d.getMinutes();
      harness.check(false, "getMinutes");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "getMinutes");
    }

    try {
      d.getSeconds();
      harness.check(false, "getSeconds");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "getSeconds");
    }

    try {
      d.setHours(0);
      harness.check(false, "setHours");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "setHours");
    }

    try {
      d.setMinutes(0);
      harness.check(false, "setMinutes");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "setMinutes");
    }

    try {
      d.setSeconds(0);
      harness.check(false, "setSeconds");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "setSeconds");
    }

    try {
      Date.valueOf("NoSuchDate");
      harness.check(false, "valueOf");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "valueOf");
    }
}
}

