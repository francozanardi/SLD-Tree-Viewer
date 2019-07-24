package com.gmail.francozanardi97.app.controller;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;

import javax.servlet.http.HttpServletRequest;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.ResourceUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.context.ContextLoader;

import form.ProgramaUsuario;
import treeSLD.TreeSLD;

@Controller
@RequestMapping(value="/")
public class InicioController {
	
//	@Autowired mejor no lo hacemos un bean, creemos que no es correcto.
	private TreeSLD generadorTree;

	@RequestMapping(method=RequestMethod.GET)
	public String prepararForm(Model model) {
		model.addAttribute("programaUsuario", new ProgramaUsuario());

		return "inicio";
	}
	
	@RequestMapping(params="createSLD", method=RequestMethod.POST)
	public String crearSLD(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model, HttpServletRequest req) {
		// Por ahora la validación de errores la hacemos acá, pero se puede separar y usar otra clase, creo que sería más correcto. Leer spring.pdf.
		
		if(p.getSourceCode() == "") {
			result.rejectValue("sourceCode", "codeEmpty");
		}
		
		if(p.getQueryProlog() == "") {
			result.rejectValue("queryProlog", "consultaVacia");
		}
		
		
		if(!result.hasErrors()) {
			generadorTree = new TreeSLD(p);
			try {
				model.addAttribute("svgTreeSLD", generadorTree.generarSVG(""));
				String path = req.getServletContext().getRealPath("/prolog/sourceUser.pl");
				File f = new File(path);
				FileWriter w = new FileWriter(f);
				w.append(p.getSourceCode());
				w.flush();
				w.close();
				
				System.out.println("path: " + path);
				
				String pathPL = req.getServletContext().getRealPath("/prolog/sld_tree.pl");
				Query q1 = 
			            new Query( 
			                "consult", 
			                new Term[] {new Atom(pathPL)} 
			            );
				
				System.out.println("q1.hasSolution(): " + q1.hasSolution());
				
				q1.close();
				
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		return "inicio";
	}
	
	@RequestMapping(params="nextStep")
	public String avanzarSLD(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model) {
//		model.addAttribute("hayNext", new Boolean(true));
		System.out.println("Next presionado1");
		model.addAttribute("seUsoNext", new Boolean(true));
		
		generadorTree = new TreeSLD(null);
		model.addAttribute("svgTreeSLD", generadorTree.generarSVG(""));
		
		return "inicio";
	}
	
//	@RequestMapping(params="options", method=RequestMethod.POST)
//	public String abrirOpciones(@ModelAttribute("programaUsuario") ProgramaUsuario p, BindingResult result, Model model) {
//
//		return "inicio";
//	}
}
