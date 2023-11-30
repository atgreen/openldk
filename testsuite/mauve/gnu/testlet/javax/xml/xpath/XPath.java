// Tags: JDK1.5
//
// Copyright (C) 2006 Stephane Mikay <stephane@mikaty.net>
//
// This file is part of Mauve.
//
// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// Mauve is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301 USA.

package gnu.testlet.javax.xml.xpath;

import java.io.StringReader;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * JSR 206 (JAXP 1.3) XPath Tests
 */
public class XPath implements Testlet {

    public void test( TestHarness harness ) {
        try {
            // According to the JAXP 1.3 spec Chapter 13 Section 2.2.4
            // The XPath 1.0 NodeSet data type Maps to Java org.w3c.dom.NodeList
            harness.checkPoint( "jaxp-1.3-ch13s2.2.4" );
            harness.check( nodeset( "//*", "<o/>" ) instanceof NodeList );
        } catch ( XPathExpressionException e ) {
            harness.debug( e );
            harness.check( false );
        }
    }

    private Object nodeset( String expr, String document )
            throws XPathExpressionException {
        return eval( expr, document, XPathConstants.NODESET );
    }

    private Object eval( String expr, String document, QName returnType )
            throws XPathExpressionException {
        final XPathFactory factory = XPathFactory.newInstance();
        final javax.xml.xpath.XPath xpath = factory.newXPath();
        return xpath.evaluate( expr, source( document ), returnType );
    }

    private InputSource source( String xml ) {
        return new InputSource( new StringReader( xml ) );
    }

}
