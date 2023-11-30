// Tags: JDK1.1

// Copyright (C) 2005 Mark J. Wielaard <mark@klomp.org>

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
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.datatransfer.Clipboard;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.datatransfer.*;

public class clipboard
  extends Clipboard
  implements Testlet, Transferable, ClipboardOwner
{

  public clipboard()
  {
    super("mauve");
  }

  private clipboard(String name)
  {
    super(name);
  }

  public void test(TestHarness harness)      
  {
    harness.check(this.getName(), "mauve");
    harness.check(this.contents, null);
    harness.check(this.getContents(null), this.contents);
    harness.check(this.owner, null);

    // Claim ownership of the clipboard.
    this.setContents(this, this);
    harness.check(this.contents, this);
    harness.check(this.getContents(null), this.contents);
    harness.check(this.owner, this);
    harness.check(lostOwnerCalled, false);

    // Make someone else the owner.
    clipboard cp2 = new clipboard("dummy2");
    this.setContents(cp2, cp2);
    harness.check(lostOwnerCalled, true);
    harness.check(this.contents, cp2);
    harness.check(this.getContents(null), this.contents);
    harness.check(this.owner, cp2);
    harness.check(lostOwnerClipboard, this);
    harness.check(lostOwnerTransferable, this);
  
    // Set owner/content back to this.
    lostOwnerCalled = false;
    this.setContents(this, this);
    harness.check(lostOwnerCalled, false);
    harness.check(this.contents, this);
    harness.check(this.getContents(null), this.contents);
    harness.check(this.owner, this);

    // Keep outself as owner, but change the content.
    this.setContents(cp2, this);
    harness.check(lostOwnerCalled, false);
    harness.check(this.contents, cp2);
    harness.check(this.getContents(null), this.contents);
    harness.check(this.owner, this);
  }

  // Transferable 
  public DataFlavor[] getTransferDataFlavors()
  {
    return new DataFlavor[0];
  }

  public boolean isDataFlavorSupported(DataFlavor flavor)
  {
    return false;
  }

  public Object getTransferData(DataFlavor flavor)
  {
    return null;
  }

  // ClipboardOwner
  private boolean lostOwnerCalled = false;
  private Clipboard lostOwnerClipboard = null;
  private Transferable lostOwnerTransferable = null;
  public void lostOwnership(Clipboard clipboard, Transferable contents)
  {
    lostOwnerCalled = true;
    lostOwnerClipboard = clipboard;
    lostOwnerTransferable = contents;
  }

  // For debug readability
  public String toString()
  {
    return "[name=" + getName() + "]";
  }
}
