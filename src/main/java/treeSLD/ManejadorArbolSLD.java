package treeSLD;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;

import form.ProgramaUsuario;

public class ManejadorArbolSLD {
	
	public ManejadorArbolSLD() {
	}
	
	public void cargarSourceCode(String path, ProgramaUsuario programa) {
		File f = new File(path);
		try {
			FileWriter w = new FileWriter(f);
			w.append(programa.getSourceCode());
			w.flush();
			w.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void cargarMetainterprete(String path) {
		Query q = 
		        new Query( 
		            "consult", 
		            new Term[] {new Atom(path)} 
		        );
		
		System.out.println("q.hasSolution(): " + q.hasSolution());
		//esto no solo verifica si existe una solución, sino que también abre la solución encontrada.
		//por lo tanto es necesario hacerlo.
		q.close();
		
	}
}
