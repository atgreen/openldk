/* constants.java -- Tests for the constants in the StyleConstants class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
  
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2

package gnu.testlet.javax.swing.text.StyleConstants;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.StyleConstants;
import javax.swing.text.StyleConstants.CharacterConstants;
import javax.swing.text.StyleConstants.ColorConstants;
import javax.swing.text.StyleConstants.FontConstants;
import javax.swing.text.StyleConstants.ParagraphConstants;

/**
 * Some checks for the constants in the {@link StyleConstants} class.
 */
public class constants implements Testlet
{
    
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.check(StyleConstants.ComponentElementName.equals("component"));
    harness.check(StyleConstants.IconElementName.equals("icon"));
    harness.check(StyleConstants.ModelAttribute.toString(), "model");  
    harness.check(StyleConstants.NameAttribute.toString(), "name");  
    harness.check(StyleConstants.ResolveAttribute.toString(), "resolver");  
      
    harness.check(StyleConstants.Background.equals(
            CharacterConstants.Background));
    harness.check(StyleConstants.BidiLevel.equals(
            CharacterConstants.BidiLevel));
    harness.check(StyleConstants.Bold.equals(CharacterConstants.Bold));
    harness.check(StyleConstants.ComponentAttribute.equals(
            CharacterConstants.ComponentAttribute));
    harness.check(StyleConstants.Family.equals(CharacterConstants.Family));
    harness.check(StyleConstants.Foreground.equals(
            CharacterConstants.Foreground));
    harness.check(StyleConstants.IconAttribute.equals(
            CharacterConstants.IconAttribute));
    harness.check(StyleConstants.Italic.equals(CharacterConstants.Italic));
    harness.check(StyleConstants.Size.equals(CharacterConstants.Size));
    harness.check(StyleConstants.StrikeThrough.equals(
            CharacterConstants.StrikeThrough));
    harness.check(StyleConstants.Subscript.equals(
            CharacterConstants.Subscript));
    harness.check(StyleConstants.Superscript.equals(
            CharacterConstants.Superscript));
    harness.check(StyleConstants.Underline.equals(
            CharacterConstants.Underline));

    harness.check(StyleConstants.Background.equals(ColorConstants.Background));
    harness.check(StyleConstants.Foreground.equals(ColorConstants.Foreground));

    harness.check(StyleConstants.Bold.equals(FontConstants.Bold));
    harness.check(StyleConstants.Family.equals(FontConstants.Family));
    harness.check(StyleConstants.Italic.equals(FontConstants.Italic));
    harness.check(StyleConstants.Size.equals(FontConstants.Size));

    harness.check(StyleConstants.Alignment.equals(
            ParagraphConstants.Alignment));
    harness.check(StyleConstants.FirstLineIndent.equals(
            ParagraphConstants.FirstLineIndent));
    harness.check(StyleConstants.LeftIndent.equals(
            ParagraphConstants.LeftIndent));
    harness.check(StyleConstants.LineSpacing.equals(
            ParagraphConstants.LineSpacing));
    harness.check(StyleConstants.Orientation.equals(
            ParagraphConstants.Orientation));
    harness.check(StyleConstants.RightIndent.equals(
            ParagraphConstants.RightIndent));
    harness.check(StyleConstants.SpaceAbove.equals(
            ParagraphConstants.SpaceAbove));
    harness.check(StyleConstants.SpaceBelow.equals(
            ParagraphConstants.SpaceBelow));
    harness.check(StyleConstants.TabSet.equals(ParagraphConstants.TabSet));    
  }

}