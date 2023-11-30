/* AudioProperties.java -- simple test to read audio properties from
   sound files.
   Copyright (C) 2007 Mario Torre <neugens@limasoftware.net>
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

// Tags: JDK1.4

package gnu.testlet.javax.sound.sampled;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.UnsupportedAudioFileException;

import gnu.testlet.ResourceNotFoundException;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * @author Mario Torre <neugens@limasoftware.net>
 */
public class AudioProperties implements Testlet
{
  private static final String BASE_PATH =
    "gnu#testlet#javax#sound#sampled#data#";
  private static final String WAV = BASE_PATH + "k3b_success1.wav";
  private static final String AU = BASE_PATH + "k3b_success1.au";
  
  protected TestHarness harness = null;

  public void test(TestHarness harness)
  {
    this.harness = harness;
    this.testWav();
    this.testAU();
  }
  
  private void processWaveStream(AudioInputStream stream)
  {
    AudioFormat format = stream.getFormat();

    // NOTE: we don't check for encoding, because our backend is unable
    // to get the correct encoding as defined by AudioFormat.Encoding
    // this is not a problem, because the encodings specified do not
    // make sense in most cases.
    this.harness.check(format.getFrameSize() == 1);        
    this.harness.check(format.getChannels() == 1);
    this.harness.check(format.getSampleRate() == 8000.0);
    this.harness.check(format.getFrameRate() == 8000.0);
    this.harness.check(format.getSampleSizeInBits() == 8);
  }
  
  private void processAUStream(AudioInputStream stream)
  {
    AudioFormat format = stream.getFormat();

    // NOTE: we don't check for encoding, because our backend is unable
    // to get the correct encoding as defined by AudioFormat.Encoding
    // this is not a problem, because the encodings specified do not
    // make sense in most cases.
    this.harness.check(format.getFrameSize() == 2);        
    this.harness.check(format.getChannels() == 1);
    this.harness.check(format.getSampleRate() == 8000.0);
    this.harness.check(format.getFrameRate() == 8000.0);
    this.harness.check(format.getSampleSizeInBits() == 16);
  }
  
  private AudioInputStream getAudioStream(String filepath, boolean stream)
    throws IOException, UnsupportedAudioFileException
  {
    File file = null;
    try
      {
        file = this.harness.getResourceFile(filepath);
      }
    catch (ResourceNotFoundException e1)
      {
        throw new IOException("ResourceNotFoundException: check the correct " +
                              "input file location");
      }
    
    AudioInputStream audioInputStream = null;  
    if (stream)
      {
        audioInputStream =
          AudioSystem.getAudioInputStream(new FileInputStream(file));
      }
    else
      {
        audioInputStream = AudioSystem.getAudioInputStream(file);
      }
    
    return audioInputStream;
  }
  
  /**
   * Read a wav file and check if the expected properties match
   * the actual result.
   */
  private void testWav()
  {
    this.harness.checkPoint("testWav()");
     
    try
      {
        this.harness.checkPoint("testWav() - FILE");
        AudioInputStream audioInputStream = getAudioStream(WAV, false);
        
        processWaveStream(audioInputStream);
        
        this.harness.checkPoint("testWav() - STREAM");
        AudioInputStream audioInputStream2 = getAudioStream(WAV, true);
        
        processWaveStream(audioInputStream2);
      }
    catch (UnsupportedAudioFileException e)
      {
        this.harness.fail("Wave files should be supported by any" +
                          " implementation");
      }
    catch (IOException e)
      {
        this.harness.fail(e.getMessage());
      }
  }
  
  private void testAU()
  {
    this.harness.checkPoint("testAU()");
    
    try
      {
        this.harness.checkPoint("testAU() - FILE");
        AudioInputStream audioInputStream = getAudioStream(AU, false);
      
        processAUStream(audioInputStream);
      
        this.harness.checkPoint("testAU() - STREAM");
        AudioInputStream audioInputStream2 = getAudioStream(AU, true);
      
        processAUStream(audioInputStream2);
      }
    catch (UnsupportedAudioFileException e)
      {
        this.harness.fail("AU files should be supported by any" +
                        " implementation");
      }
    catch (IOException e)
      {
        this.harness.fail(e.getMessage());
      }
  }
}
