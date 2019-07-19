package treeSLD;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.geom.CubicCurve2D;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.apache.batik.svggen.SVGGraphics2D;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.svg.SVGDocument;

import form.ProgramaUsuario;

public class TreeSLD {
	private ProgramaUsuario programa;
	
	public TreeSLD(ProgramaUsuario p) {
		programa = p;
	}
	
	public String generarSVG() {
		
		DOMImplementation impl = SVGDOMImplementation.getDOMImplementation();
		String svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI;
		Document doc = impl.createDocument(svgNS, "svg", null);

		// Get the root element (the 'svg' element).
		Element svgRoot = doc.getDocumentElement();

		// Set the width and height attributes on the root 'svg' element.
		svgRoot.setAttributeNS(null, "width", "400");
		svgRoot.setAttributeNS(null, "height", "450");

		// Create the rectangle.
		Element rectangle = doc.createElementNS(svgNS, "rect");
		rectangle.setAttributeNS(null, "x", "10");
		rectangle.setAttributeNS(null, "y", "20");
		rectangle.setAttributeNS(null, "width", "100");
		rectangle.setAttributeNS(null, "height", "50");
		rectangle.setAttributeNS(null, "fill", "blue");

		// Attach the rectangle to the root 'svg' element.
		svgRoot.appendChild(rectangle);
		
		SVGGraphics2D graphics = new SVGGraphics2D(doc);
        boolean useCSS = true; // we want to use CSS style attribute
        Writer w = new StringWriter();
        try {
			graphics.stream(doc.getDocumentElement(), w, useCSS, false);
	        w.flush();
	        w.close();
	        System.out.println("éxito1");
	        return w.toString();
		} catch (IOException e) {
			e.printStackTrace();
			
			return "Ha ocurrido un error interno";
		}       

	}
	
	public String generarSVG2() {
		
        DOMImplementation domImpl =
	    SVGDOMImplementation.getDOMImplementation();
	
	    // Create a document with the appropriate namespace
	    SVGDocument document = 
	    	(SVGDocument) domImpl.createDocument(SVGDOMImplementation.SVG_NAMESPACE_URI, "svg", null);
	
	    // Create an instance of the SVG Generator
	    SVGGraphics2D graphics = new SVGGraphics2D(document);
	    // draw onto the SVG Graphics object
	    graphics.setSVGCanvasSize(new Dimension(500,500));
	    graphics.setColor(Color.BLACK);
	    graphics.drawRect(190,190,60,60);
	    graphics.setFont(Font.decode("Courier-bold-20"));
	    graphics.drawString("An SVG document ceated with Batik", 40, 100);
	    graphics.setColor(Color.BLUE);
	    
	    CubicCurve2D curve = new CubicCurve2D.Double(150, 150, 175, 125, 300, 175, 300, 300); 
	    graphics.draw(curve);
	    graphics.drawOval(200,200,40,40);
	    graphics.setColor(Color.RED);
	    graphics.drawOval(100,180,60,80);
		
        boolean useCSS = true; // we want to use CSS style attribute
        Writer w = new StringWriter();
        try {
			graphics.stream(w, useCSS);
	        w.flush();
	        w.close();
	        System.out.println("éxito2");
	        return w.toString();
		} catch (IOException e) {
			e.printStackTrace();
			
			return "Ha ocurrido un error interno";
		}       

	}
}
