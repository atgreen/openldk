/*************************************************************************
/* TimeTest.java - Test java.sql.Time
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

package gnu.testlet.java.sql.Time;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.sql.*;

public class TimeTest implements Testlet
{

public void
test(TestHarness harness)
{
    Time t = new Time(0);

    try {
      t.getDate();
      harness.check(false, "getDate");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "getDate");
    }

    try {
      t.getDay();
      harness.check(false, "getDay");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "getDay");
    }

    try {
      t.getMonth();
      harness.check(false, "getMonth");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "getMonth");
    }

    try {
      t.getYear();
      harness.check(false, "getYear");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "getYear");
    }

    try {
      t.setDate(0);
      harness.check(false, "setDate");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "setDate");
    }

    try {
      t.setMonth(0);
      harness.check(false, "setMonth");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "setMonth");
    }

    try {
      t.setYear(0);
      harness.check(false, "setYear");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "setYear");
    }

    try {
      Time.valueOf("NoSuchTime");
      harness.check(false, "valueOf");
    }
    catch (IllegalArgumentException e) {
      harness.check(true, "valueOf");
    }
}
}

