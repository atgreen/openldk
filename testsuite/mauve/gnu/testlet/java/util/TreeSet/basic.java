// Tags: JDK1.2

// Copyright (C) 2004, 2005 Audrius Meskauskas <audriusa@bluewin.ch>

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


package gnu.testlet.java.util.TreeSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * Basic TreeSet test.
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class basic implements Testlet
{
  TreeSet set = new TreeSet();

  void checkContent(Set forSet, String content, TestHarness h, String note)
  {
    StringBuffer b = new StringBuffer();
    Iterator iter = forSet.iterator();
    while (iter.hasNext())
      {
        b.append(iter.next());
      }

    h.check(b.toString(), content, note);
  }

  void checkContent(String content, TestHarness h, String note)
  {
    checkContent(set, content, h, note);
  }

  TreeSet getSet(String content)
  {
    TreeSet t = new TreeSet();

    for (int i = 0; i < content.length(); i++)
      {
        t.add("" + content.charAt(i));
      }

    return t;
  }

  /* Test clone(). */
  public void test_clone(TestHarness harness)
  {
    TreeSet t = getSet("abcdef");
    set = (TreeSet) t.clone();
    checkContent("abcdef", harness, "clone");
  }

  /* Test add(Object). */
  public void test_add(TestHarness harness)
  {
    set = getSet("bcdabcddabbccaabbccadbcdababbcdabcxabcxccda");
    checkContent("abcdx", harness, "add");
    harness.check(set.size(), 5, "size");
    harness.check(set.first(), "a", "first");
    harness.check(set.last(), "x", "last");
    harness.check(set.comparator() == null, "null comparator expected");
  }

  /* Test addAll(Collection). */
  public void test_addAll(TestHarness harness)
  {
    set = getSet("dac");

    TreeSet t = getSet("xay");

    set.addAll(t);

    checkContent("acdxy", harness, "addAll");
  }

  /* Test contains(Object). */
  public void test_contains(TestHarness harness)
  {
    String t = "abcdefghij";
    set = getSet(t);

    for (int i = 0; i < t.length(); i++)
      {
        String s = t.substring(i, i + 1);
        harness.check(set.contains(s), "must contain '" + s + "'");
      }

    harness.check(!set.contains("aa"), "must not contain 'aa'");
  }

  /* Test remove(Object). */
  public void test_remove(TestHarness harness)
  {
    String t = "abcdefghij";
    set = getSet(t);

    for (int i = 0; i < t.length(); i++)
      {
        String s = t.substring(i, i + 1);
        set.remove(s);

        if (set.contains(s))
          harness.fail("Contains '" + s + "' after removing. ");
      }

    harness.check(set.size(), 0, "non zero size after removing all elements");

    harness.check(set.isEmpty(), "non empty when it should be");
  }

  /* Test clear(). */
  public void test_clear(TestHarness harness)
  {
    set = getSet("a");
    set.clear();
    harness.check(set.size(), 0, "clear");
  }

  /* Test headSet(Object). */
  public void test_subsets(TestHarness harness)
  {
    String content = "abcdefghijklmn";

    set = getSet(content);

    for (int i = 0; i < content.length() - 1; i++)
      {
        String s = content.substring(i, i + 1);
        checkContent(set.headSet(s), content.substring(0, i), harness, "headSet");

        checkContent(set.tailSet(s), content.substring(i), harness, "tailSet");

        checkContent(set.subSet(s, "n"),
                     content.substring(i, content.length() - 1), harness,
                     "subset"
                    );
      }
  }

  public void test(TestHarness harness)
  {
    test_clone(harness);
    test_add(harness);
    test_addAll(harness);
    test_contains(harness);
    test_remove(harness);
    test_clear(harness);
    test_subsets(harness);
  }
}
