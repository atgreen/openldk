/* testToString.java -- 
   Copyright (C) 2006 RedHat
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

// Tags: 1.3

package gnu.testlet.java.awt.MenuShortcut;

import java.awt.MenuShortcut;
import java.awt.event.KeyEvent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class testToString implements Testlet
{

  public void test(TestHarness harness)
  {
    MenuShortcut m = new MenuShortcut(KeyEvent.VK_ENTER);
    harness.check(m.toString(), "Ctrl+Enter");
    harness.check(m.usesShiftModifier(), false);
    
    m = new MenuShortcut(KeyEvent.VK_BACK_SPACE, true);
    harness.check(m.toString(), "Ctrl+Shift+Backspace");
    harness.check(m.usesShiftModifier(), true);
    
    m = new MenuShortcut(KeyEvent.VK_TAB);
    harness.check(m.toString(), "Ctrl+Tab");
    
    m = new MenuShortcut(KeyEvent.VK_CANCEL);
    harness.check(m.toString(), "Ctrl+Cancel");
    
    m = new MenuShortcut(KeyEvent.VK_CLEAR);
    harness.check(m.toString(), "Ctrl+Clear");
    
    m = new MenuShortcut(KeyEvent.VK_SHIFT);
    harness.check(m.toString(), "Ctrl+Shift");
    
    m = new MenuShortcut(KeyEvent.VK_CONTROL);
    harness.check(m.toString(), "Ctrl+Ctrl");
    
    m = new MenuShortcut(KeyEvent.VK_ALT);
    harness.check(m.toString(), "Ctrl+Alt");
    
    m = new MenuShortcut(KeyEvent.VK_PAUSE);
    harness.check(m.toString(), "Ctrl+Pause");
    
    m = new MenuShortcut(KeyEvent.VK_CAPS_LOCK);
    harness.check(m.toString(), "Ctrl+Caps Lock");
    
    m = new MenuShortcut(KeyEvent.VK_ESCAPE);
    harness.check(m.toString(), "Ctrl+Escape");
    
    m = new MenuShortcut(KeyEvent.VK_SPACE);
    harness.check(m.toString(), "Ctrl+Space");
    
    m = new MenuShortcut(KeyEvent.VK_PAGE_UP);
    harness.check(m.toString(), "Ctrl+Page Up");
    
    m = new MenuShortcut(KeyEvent.VK_PAGE_DOWN);
    harness.check(m.toString(), "Ctrl+Page Down");
    
    m = new MenuShortcut(KeyEvent.VK_END);
    harness.check(m.toString(), "Ctrl+End");
    
    m = new MenuShortcut(KeyEvent.VK_HOME);
    harness.check(m.toString(), "Ctrl+Home");
    
    m = new MenuShortcut(KeyEvent.VK_LEFT);
    harness.check(m.toString(), "Ctrl+Left");
    
    m = new MenuShortcut(KeyEvent.VK_UP);
    harness.check(m.toString(), "Ctrl+Up");
    
    m = new MenuShortcut(KeyEvent.VK_RIGHT);
    harness.check(m.toString(), "Ctrl+Right");
    
    m = new MenuShortcut(KeyEvent.VK_DOWN);
    harness.check(m.toString(), "Ctrl+Down");
    
    m = new MenuShortcut(KeyEvent.VK_COMMA);
    harness.check(m.toString(), "Ctrl+Comma");
    
    m = new MenuShortcut(KeyEvent.VK_PERIOD);
    harness.check(m.toString(), "Ctrl+Period");
    
    m = new MenuShortcut(KeyEvent.VK_SLASH);
    harness.check(m.toString(), "Ctrl+Slash");
    
    m = new MenuShortcut(KeyEvent.VK_SEMICOLON);
    harness.check(m.toString(), "Ctrl+Semicolon");
    
    m = new MenuShortcut(KeyEvent.VK_EQUALS);
    harness.check(m.toString(), "Ctrl+Equals");
    
    m = new MenuShortcut(KeyEvent.VK_BACK_SLASH);
    harness.check(m.toString(), "Ctrl+Back Slash");
    
    m = new MenuShortcut(KeyEvent.VK_0);
    harness.check(m.toString(), "Ctrl+0");
    
    m = new MenuShortcut(KeyEvent.VK_1);
    harness.check(m.toString(), "Ctrl+1");
    
    m = new MenuShortcut(KeyEvent.VK_2);
    harness.check(m.toString(), "Ctrl+2");
    
    m = new MenuShortcut(KeyEvent.VK_3);
    harness.check(m.toString(), "Ctrl+3");
    
    m = new MenuShortcut(KeyEvent.VK_4);
    harness.check(m.toString(), "Ctrl+4");
    
    m = new MenuShortcut(KeyEvent.VK_5);
    harness.check(m.toString(), "Ctrl+5");
    
    m = new MenuShortcut(KeyEvent.VK_6);
    harness.check(m.toString(), "Ctrl+6");
    
    m = new MenuShortcut(KeyEvent.VK_7);
    harness.check(m.toString(), "Ctrl+7");
    
    m = new MenuShortcut(KeyEvent.VK_8);
    harness.check(m.toString(), "Ctrl+8");
    
    m = new MenuShortcut(KeyEvent.VK_9);
    harness.check(m.toString(), "Ctrl+9");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD0);
    harness.check(m.toString(), "Ctrl+NumPad-0");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD1);
    harness.check(m.toString(), "Ctrl+NumPad-1");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD2);
    harness.check(m.toString(), "Ctrl+NumPad-2");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD3);
    harness.check(m.toString(), "Ctrl+NumPad-3");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD4);
    harness.check(m.toString(), "Ctrl+NumPad-4");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD5);
    harness.check(m.toString(), "Ctrl+NumPad-5");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD6);
    harness.check(m.toString(), "Ctrl+NumPad-6");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD7);
    harness.check(m.toString(), "Ctrl+NumPad-7");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD8);
    harness.check(m.toString(), "Ctrl+NumPad-8");
    
    m = new MenuShortcut(KeyEvent.VK_NUMPAD9);
    harness.check(m.toString(), "Ctrl+NumPad-9");
    
    m = new MenuShortcut(KeyEvent.VK_MULTIPLY);
    harness.check(m.toString(), "Ctrl+NumPad *");
    
    m = new MenuShortcut(KeyEvent.VK_ADD);
    harness.check(m.toString(), "Ctrl+NumPad +");
    
    m = new MenuShortcut(KeyEvent.VK_SEPARATER);
    harness.check(m.toString(), "Ctrl+NumPad ,");
    
    m = new MenuShortcut(KeyEvent.VK_SUBTRACT);
    harness.check(m.toString(), "Ctrl+NumPad -");
    
    m = new MenuShortcut(KeyEvent.VK_DECIMAL);
    harness.check(m.toString(), "Ctrl+NumPad .");
    
    m = new MenuShortcut(KeyEvent.VK_DIVIDE);
    harness.check(m.toString(), "Ctrl+NumPad /");
    
    m = new MenuShortcut(KeyEvent.VK_A);
    harness.check(m.toString(), "Ctrl+A");
    
    m = new MenuShortcut(KeyEvent.VK_B);
    harness.check(m.toString(), "Ctrl+B");
    
    m = new MenuShortcut(KeyEvent.VK_C);
    harness.check(m.toString(), "Ctrl+C");
    
    m = new MenuShortcut(KeyEvent.VK_D);
    harness.check(m.toString(), "Ctrl+D");
    
    m = new MenuShortcut(KeyEvent.VK_E);
    harness.check(m.toString(), "Ctrl+E");
    
    m = new MenuShortcut(KeyEvent.VK_F);
    harness.check(m.toString(), "Ctrl+F");
    
    m = new MenuShortcut(KeyEvent.VK_G);
    harness.check(m.toString(), "Ctrl+G");
    
    m = new MenuShortcut(KeyEvent.VK_H);
    harness.check(m.toString(), "Ctrl+H");
    
    m = new MenuShortcut(KeyEvent.VK_I);
    harness.check(m.toString(), "Ctrl+I");
    
    m = new MenuShortcut(KeyEvent.VK_J);
    harness.check(m.toString(), "Ctrl+J");
    
    m = new MenuShortcut(KeyEvent.VK_L);
    harness.check(m.toString(), "Ctrl+L");
    
    m = new MenuShortcut(KeyEvent.VK_M);
    harness.check(m.toString(), "Ctrl+M");
    
    m = new MenuShortcut(KeyEvent.VK_N);
    harness.check(m.toString(), "Ctrl+N");
    
    m = new MenuShortcut(KeyEvent.VK_O);
    harness.check(m.toString(), "Ctrl+O");
    
    m = new MenuShortcut(KeyEvent.VK_P);
    harness.check(m.toString(), "Ctrl+P");
    
    m = new MenuShortcut(KeyEvent.VK_Q);
    harness.check(m.toString(), "Ctrl+Q");
    
    m = new MenuShortcut(KeyEvent.VK_R);
    harness.check(m.toString(), "Ctrl+R");
    
    m = new MenuShortcut(KeyEvent.VK_S);
    harness.check(m.toString(), "Ctrl+S");
    
    m = new MenuShortcut(KeyEvent.VK_T);
    harness.check(m.toString(), "Ctrl+T");
    
    m = new MenuShortcut(KeyEvent.VK_U);
    harness.check(m.toString(), "Ctrl+U");
    
    m = new MenuShortcut(KeyEvent.VK_V);
    harness.check(m.toString(), "Ctrl+V");
    
    m = new MenuShortcut(KeyEvent.VK_W);
    harness.check(m.toString(), "Ctrl+W");
    
    m = new MenuShortcut(KeyEvent.VK_X);
    harness.check(m.toString(), "Ctrl+X");
    
    m = new MenuShortcut(KeyEvent.VK_Y);
    harness.check(m.toString(), "Ctrl+Y");
    
    m = new MenuShortcut(KeyEvent.VK_Z);
    harness.check(m.toString(), "Ctrl+Z");
    
    m = new MenuShortcut(KeyEvent.VK_F1);
    harness.check(m.toString(), "Ctrl+F1");
    
    m = new MenuShortcut(KeyEvent.VK_F2);
    harness.check(m.toString(), "Ctrl+F2");
    
    m = new MenuShortcut(KeyEvent.VK_F3);
    harness.check(m.toString(), "Ctrl+F3");
    
    m = new MenuShortcut(KeyEvent.VK_F4);
    harness.check(m.toString(), "Ctrl+F4");
    
    m = new MenuShortcut(KeyEvent.VK_F5);
    harness.check(m.toString(), "Ctrl+F5");
    
    m = new MenuShortcut(KeyEvent.VK_F6);
    harness.check(m.toString(), "Ctrl+F6");
    
    m = new MenuShortcut(KeyEvent.VK_F7);
    harness.check(m.toString(), "Ctrl+F7");
    
    m = new MenuShortcut(KeyEvent.VK_F8);
    harness.check(m.toString(), "Ctrl+F8");
    
    m = new MenuShortcut(KeyEvent.VK_F9);
    harness.check(m.toString(), "Ctrl+F9");
    
    m = new MenuShortcut(KeyEvent.VK_F10);
    harness.check(m.toString(), "Ctrl+F10");
    
    m = new MenuShortcut(KeyEvent.VK_F11);
    harness.check(m.toString(), "Ctrl+F11");
    
    m = new MenuShortcut(KeyEvent.VK_F12);
    harness.check(m.toString(), "Ctrl+F12");
    
    m = new MenuShortcut(KeyEvent.VK_DELETE);
    harness.check(m.toString(), "Ctrl+Delete");
    
    m = new MenuShortcut(KeyEvent.VK_NUM_LOCK);
    harness.check(m.toString(), "Ctrl+Num Lock");
    
    m = new MenuShortcut(KeyEvent.VK_SCROLL_LOCK);
    harness.check(m.toString(), "Ctrl+Scroll Lock");
    
    m = new MenuShortcut(KeyEvent.VK_PRINTSCREEN);
    harness.check(m.toString(), "Ctrl+Print Screen");
    
    m = new MenuShortcut(KeyEvent.VK_INSERT);
    harness.check(m.toString(), "Ctrl+Insert");
    
    m = new MenuShortcut(KeyEvent.VK_HELP);
    harness.check(m.toString(), "Ctrl+Help");
    
    m = new MenuShortcut(KeyEvent.VK_META);
    harness.check(m.toString(), "Ctrl+Meta");
    
    m = new MenuShortcut(KeyEvent.VK_BACK_QUOTE);
    harness.check(m.toString(), "Ctrl+Back Quote");
    
    m = new MenuShortcut(KeyEvent.VK_QUOTE);
    harness.check(m.toString(), "Ctrl+Quote");
    
    m = new MenuShortcut(KeyEvent.VK_OPEN_BRACKET);
    harness.check(m.toString(), "Ctrl+Open Bracket");
    
    m = new MenuShortcut(KeyEvent.VK_CLOSE_BRACKET);
    harness.check(m.toString(), "Ctrl+Close Bracket");
    
    m = new MenuShortcut(KeyEvent.VK_ACCEPT);
    harness.check(m.toString(), "Ctrl+Accept");
    
    m = new MenuShortcut(KeyEvent.VK_CONVERT);
    harness.check(m.toString(), "Ctrl+Convert");
    
    m = new MenuShortcut(KeyEvent.VK_FINAL);
    harness.check(m.toString(), "Ctrl+Final");
    
    m = new MenuShortcut(KeyEvent.VK_KANA);
    harness.check(m.toString(), "Ctrl+Kana");
    
    m = new MenuShortcut(KeyEvent.VK_KANJI);
    harness.check(m.toString(), "Ctrl+Kanji");
    
    m = new MenuShortcut(KeyEvent.VK_MODECHANGE);
    harness.check(m.toString(), "Ctrl+Mode Change");
    
    m = new MenuShortcut(KeyEvent.VK_NONCONVERT);
    harness.check(m.toString(), "Ctrl+No Convert");
  }

}
