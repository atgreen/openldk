//Tags: JDK1.4

//Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

//Mauve is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2, or (at your option)
//any later version.

//Mauve is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.

//You should have received a copy of the GNU General Public License
//along with Mauve; see the file COPYING.  If not, write to
//the Free Software Foundation, 59 Temple Place - Suite 330,
//Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.RenderingHints.Key;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.RenderingHints;

/**
 * Some (brute force) checks for the isCompatibleValue() method of the 
 * {@link RenderingHints.Key} class.
 */
public class isCompatibleValue implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("KEY_ALPHA_INTERPOLATION");
    RenderingHints.Key key = RenderingHints.KEY_ALPHA_INTERPOLATION;
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
    
    harness.checkPoint("KEY_ANTIALIASING");
    key = RenderingHints.KEY_ANTIALIASING;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
    
    harness.checkPoint("KEY_COLOR_RENDERING");
    key = RenderingHints.KEY_COLOR_RENDERING;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
    
    harness.checkPoint("KEY_DITHERING");
    key = RenderingHints.KEY_DITHERING;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
    
    harness.checkPoint("KEY_FRACTIONALMETRICS");
    key = RenderingHints.KEY_FRACTIONALMETRICS;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
    
    harness.checkPoint("KEY_INTERPOLATION");
    key = RenderingHints.KEY_INTERPOLATION;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
    
    harness.checkPoint("KEY_RENDERING");
    key = RenderingHints.KEY_RENDERING;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
 
    harness.checkPoint("KEY_STROKE_CONTROL");
    key = RenderingHints.KEY_STROKE_CONTROL;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));
    
    harness.checkPoint("KEY_TEXT_ANTIALIASING");
    key = RenderingHints.KEY_TEXT_ANTIALIASING;
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_COLOR_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_DISABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_DITHER_ENABLE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_FRACTIONALMETRICS_ON));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_QUALITY));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_RENDER_SPEED));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_DEFAULT));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_NORMALIZE));
    harness.check(!key.isCompatibleValue(RenderingHints.VALUE_STROKE_PURE));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
    harness.check(key.isCompatibleValue(RenderingHints.VALUE_TEXT_ANTIALIAS_ON));
    harness.check(!key.isCompatibleValue(null));

  }

}
