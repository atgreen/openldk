// Copyright (C) 2007 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>
// Based on code by Jakub Jelinek <jakub@redhat.com>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// Tags: JDK1.4

package gnu.testlet.java.util.TimeZone;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Calendar;
import java.util.Date;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class zdump implements Testlet
{
  public static final String zdump = "/usr/sbin/zdump";
  
  public void test(TestHarness harness)
  {
    String zoneinfodir = System.getProperty("gnu.java.util.zoneinfo.dir");
    if (zoneinfodir == null)
      return;

    if (!new File(zdump).exists() || !new File(zoneinfodir).isDirectory())
      return;

    TimeZone utc = (TimeZone) new SimpleTimeZone(0, "GMT");
    TimeZone.setDefault(utc);

    String[] zones = TimeZone.getAvailableIDs();
    for (int i = 0; i < zones.length; i++)
      {
	if (!new File(zoneinfodir, zones[i]).exists())
	  continue;

	// These two timezones have different definitions between
	// tzdata and JDK.  In JDK EST is EST5EDT, while in tzdata
	// just EST5, similarly for MST.
	if (zones[i].equals("EST") || zones[i].equals("MST"))
	  continue;

	checkZone(harness, zones[i]);
      }
  }

  public static void checkZone(TestHarness harness, String zone)
  {
    harness.checkPoint(zone);
    
    TimeZone tz = TimeZone.getTimeZone(zone);
    if (tz == null)
      {
	harness.check(false);
	return;
      }

    Calendar cal = Calendar.getInstance(tz);

    BufferedReader br = null;
    Process process = null;
    try
      {
	process = Runtime.getRuntime().exec(zdump + " -v " + zone);
	br = new BufferedReader(new InputStreamReader(
	  process.getInputStream()));
	    
	for (String line = br.readLine(); line != null; line = br.readLine())
	  {
	    int end1 = line.indexOf(" UTC = ");
	    if (end1 < 0)
	      continue;
	    int start1 = line.indexOf("  ");
	    if (start1 < 0 || start1 >= end1)
	      continue;
	    int start2 = line.indexOf(" isdst=");
	    int start3 = line.indexOf(" gmtoff=");
	    if (start2 <= end1 || start3 <= start2)
	      continue;

	    Date d = new Date(line.substring(start1 + 2, end1 + 4));
	    cal.setTime(d);

	    int isdst = Integer.parseInt(line.substring(start2 + 7, start3));
	    int gmtoff = Integer.parseInt(
	      line.substring(start3 + 8, line.length()));

	    harness.debug("Zone " + zone + " " + d +
			  " isdst=" + isdst +
			  " inDaylightTime=" + tz.inDaylightTime(d));
	    harness.check(tz.inDaylightTime(d) == (isdst != 0));

	    harness.debug("Zone " + zone + " " + d +
			  " gmtoff=" + gmtoff +
			  " getOffset=" + tz.getOffset(d.getTime()));
	    harness.check(tz.getOffset(d.getTime()) == gmtoff * 1000);

	    int offset = cal.get(Calendar.DST_OFFSET) +
	      cal.get(Calendar.ZONE_OFFSET);
	    
	    harness.debug("Zone " + zone + " " + d +
			  " gmtoff=" + gmtoff +
			  " DST_OFFSET+ZONE_OFFSET=" + offset);
	    harness.check(offset == gmtoff * 1000);
	  }
      }
    catch (IOException ioe)
      {
      }
    finally
      {
	try
	  {
	    if (br != null)
	      br.close();
	    if (process != null)
	      {
		process.waitFor();
		process.exitValue();
	      }
	  }
	catch (IOException ioe)
	  {
	  }
	catch (InterruptedException ine)
	  {
	  }
      }
  }
}
