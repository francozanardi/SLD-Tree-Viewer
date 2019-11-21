package treeSLD;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
import java.util.Stack;
import java.util.TreeMap;

public class GeneradorSVG {
	//private ArrayList<Integer> columnasPorFila;
	/*
	 * En este arrayList se guardarán la cantidad de columnas en cada fila.
	 * Es decir, columnasPorFilas.get(0) siempre será igual a 1, ya que habrá una única columna en la primer fila, la cual será la del primer nodo.
	*/
	
	private PriorityQueue<NodoTree> nodosNext;
	private PriorityQueue<RamaTree> ramasNext;
	
	private Stack<NodoTree> nodosPrev;
	private Stack<RamaTree> ramasPrev;
	
	private int anchoColumna; //tam de una columna
	private int altoFila; //tam de una fila
	private int cantidadColumnas;
	private int cantidadFilas;
	private int fotogramaActual;
	
	/*
	 * ancho := anchoColumna*(cantidadColumnas+1)
	 * alto := altoFila*(cantidadFilas+1)
	*/
	private final int ALTO_FILA_MIN = 10;
	
	/*private final int SEPARACION_MIN_X = 10;
	private final int SEPARACION_MIN_Y = 10;*/
	
	public GeneradorSVG() {
		nodosNext = new PriorityQueue<NodoTree>(new ComparadorElementos<NodoTree>());
		ramasNext = new PriorityQueue<RamaTree>(new ComparadorElementos<RamaTree>());
		
		nodosPrev = new Stack<NodoTree>();
		ramasPrev = new Stack<RamaTree>();
		
		anchoColumna = 0;
		altoFila = ALTO_FILA_MIN;
		cantidadColumnas = 1;
		cantidadFilas = 1;
		fotogramaActual = 0;
	}
	
	public void agregarNodo(NodoTree n) {
		nodosNext.add(n);
	}
	
	public void agregarRama(RamaTree r) {
		ramasNext.add(r);
	}
	
	private String crearSVG() {
		if(nodosNext.isEmpty() || (!nodosPrev.isEmpty() && !ramasPrev.isEmpty())) {
			return "";
		}
		

		NodoTree n = nodosNext.poll();
		
		//acá a partir de n tenemos que generar el nodo de la consulta gráficamente.
		fotogramaActual++;
		return ""; 
	}
	

	public String nextSVG() {
		if(nodosPrev.isEmpty() && ramasPrev.isEmpty()) {
			return crearSVG();
		}
		
		List<RamaTree> ramasActuales = new LinkedList<>();
		List<NodoTree> nodosActuales = new LinkedList<>();
		
		while(ramasNext.peek().getFotograma() == fotogramaActual)
			ramasActuales.add(ramasNext.poll());
		
		while(nodosNext.peek().getFotograma() == fotogramaActual)
			nodosActuales.add(nodosNext.poll());
		
		
		
		fotogramaActual++;
		return "";
	}
	
	/*
	 * A partir de una lista de ramas obtiene la mayor cantidad de ramas que poseen el mismo nodo de padre.
	 */
	private int getMultiplicador(List<RamaTree> ramas) {
		Map<NodoTree, Integer> cantHijos = new HashMap<NodoTree, Integer>();
		

		for(RamaTree rama : ramas) {
			NodoTree nodoPadreActual = rama.getPadre();
			if(cantHijos.get(nodoPadreActual) == null) {
				cantHijos.put(nodoPadreActual, 1);
			} else {
				cantHijos.put(nodoPadreActual, cantHijos.get(nodoPadreActual) + 1);
			}
		}
		
		int multiplicador = 0;
		for(Integer i : cantHijos.values()) {
			if(i > multiplicador) {
				multiplicador = i;
			}
		}
		
		return multiplicador;
	}
	
}
