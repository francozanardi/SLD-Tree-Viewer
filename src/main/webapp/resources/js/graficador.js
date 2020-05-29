var myID = null;
var treant = null;
var mapNodos = new Map(); //mapeo desde id nodo a objeto TreeNode
var mapRamasDisponibles = new Map();
//mapeo que contiene solo las ramas sin nodos hijos,
//es decir el mapeo va desde id rama a objeto TreeNode, el cual es un nodo invisible.
var cantSoluciones = 0;

const SPEED_ANIMATION = 500;

var configTreeDefault = {
	chart: {
		container: "#sldtree",
		scrollbar: "fancy",
		
		animation: {
			nodeAnimation: "easeOutBounce",
			nodeSpeed: SPEED_ANIMATION,
			connectorsAnimation: "bounce",
			connectorsSpeed: SPEED_ANIMATION
		},
		
		callback: {		
			onCreateNode: function(newNode, newNodeDOM) {
				var redraw = false;
				var esperando = false;
				const MAX_WIDTH = '150px';
				const OVERFLOW = 'hidden';
				
				var pdom = $(newNodeDOM.querySelector('p.node-name'));
				pdom.css({'overflow' : OVERFLOW});
				pdom.css({'max-width' : MAX_WIDTH});
				newNode.width = pdom.outerWidth();
				
				$(newNodeDOM.querySelector('p.node-name')).hover(function () {
					var self = this;
					esperando = true;	
					
					setTimeout(function() {
						if(esperando){
							var wOrig = $(self).outerWidth();
							
							$(self).css({'overflow' : 'visible'});
							$(self).css({'max-width' : 'fit-content'});
							
							if($(self).width() > wOrig){
								newNode.width = $(self).outerWidth();
								newNode.getTree().redraw();
								redraw = true;
							}
							
							esperando = false;
						}
					}, Math.max(newNode.getTree().CONFIG.animation.nodeSpeed, newNode.getTree().CONFIG.animation.connectorsSpeed));
						

				}, function () {
					if(!esperando){
						$(this).css({'overflow' : OVERFLOW});
						$(this).css({'max-width' : MAX_WIDTH});
						
						if(redraw){
							newNode.width = $(this).outerWidth();
							newNode.getTree().redraw();
							redraw = false;
						}
					}
					
					esperando = false;
				});
			},
	
		},
		
		connectors: {
			type: 'straight'
		},
		
		node: {
			collapsable: false
		},
		
	},
	
	nodeStructure: {
	}
	
};

export function getUltimoNodo(){
    var ultimoNodo = Array.from(mapNodos.values()).pop();
	return {rotulo: ultimoNodo.text.name};
}

export function getCantidadNodos(){
    return mapNodos.size;
}

export function getID(){
	return myID;
}

export function esNodoSolucion(nodo){
	if(nodo === undefined){
		return false;
	}

	return nodo.rotulo === "[]";
}

export function arbolFinalizado(ultimoNodo){
	if(ultimoNodo === undefined){
		return false;
	}

	return (esNodoSolucion(ultimoNodo) || ultimoNodo.rotulo === "[fail]") && mapRamasDisponibles.size === 0;
}

export function redrawTree(){
	treant.tree.redraw();
}


export function quitarAnimacion(){
    treant.tree.CONFIG.animation.connectorsSpeed = 0;
    treant.tree.CONFIG.animation.nodeSpeed = 0;
}

export function restoreSpeedAnimation(){
    treant.tree.CONFIG.animation.connectorsSpeed = SPEED_ANIMATION;
    treant.tree.CONFIG.animation.nodeSpeed = SPEED_ANIMATION;
}

export function getCantidadSoluciones(){
    return cantSoluciones;
}

export function agregarSolucion(nodo){
	cantSoluciones++;
	$.post('getSolucion', 'id='+myID, (solucion) => {
		
		if(solucion && solucion.rotulo !== "" && solucion.rotulo !== "[]"){
			var nodoSolucion = {
					text: {
						name: solucion.rotulo
					}
			};
			
			treant.tree.addNode(mapNodos.get(nodo.id), nodoSolucion);
		}

	});
	

}


export function nextFotograma(callback){
	var argCallback;

	$.post('avanzarFotograma', 'id='+myID)
	.then(() => {
		return $.post('getRamas', 'id='+myID);
	})
	.then(ramas => {
		console.log('ramas: ', ramas);
		
		graficarRamas(ramas);
		return $.post('getRamasCut', 'id='+myID);
	})
	.then(ramasCut => {
		console.log('ramasCut: ', ramasCut);
		
		graficarRamasCut(ramasCut);
		return $.post('getNodos', 'id='+myID);
	})
	.then(nodos => {
		argCallback = nodos;
		console.log('nodos: ', nodos);
		
		graficarNodos(nodos);
		return $.post('getSustituciones', 'id='+myID);
	})
	.then(sustituciones => {
		console.log('sustituciones: ', sustituciones);
		console.log('nodos de nuevo: ', argCallback);
		
		graficarSustituciones(sustituciones);

		if(callback && typeof(callback) === "function"){
			callback(argCallback);
		}
	})


}

export function crearArbol(success, error){
			var s = "sourceCode=" + encodeURIComponent(editor.getValue()) + "&queryProlog="
					+ encodeURIComponent($('#queryProlog').val());

			$.post('', s)
				.then(id => {
					myID = id;
					
					return $.post('getRaiz', 'id='+myID);
				})
				.then(raiz => {
                    graficarInicioArbol(raiz);
                    
                    if(success && typeof(success) === "function"){
                        success();
                    }
				})
				.catch((errorName) => {
                    if(error && typeof(error) === "function"){
                        error(errorName);
                    }
				})
}


function graficarInicioArbol(nodo){
	console.log('nodo.rotulo: ', nodo.rotulo);
	
	configTreeDefault.nodeStructure.text = {
		name: nodo.rotulo
	};

	configTreeDefault.nodeStructure.HTMLid = 'nodo_' + nodo.id;
	
	if(treant){
		treant.tree.reload();
	} else {
		treant = new Treant(configTreeDefault, null, $);
	}

	mapNodos.set(0, treant.tree.nodeDB.db[0]);
	
}


export function eliminarArbol(callback){
	$.post('eliminarArbol', 'id='+myID)
	.always(() => {		
		mapRamasDisponibles.clear();
		mapNodos.clear();
		cantSoluciones = 0;
      
		myID = null;

		configTreeDefault.nodeStructure = {HTMLclass: "nodo_invisible"};
		
		treant.tree.reload();
        configTreeDefault.nodeStructure = {};
        
        if(callback && typeof(callback) === "function"){
			callback();
		}
	});
}

function graficarRamas(ramas){
	for(let i in ramas){
		let rama = ramas[i];
		
		let nodeInvisible = {
				text: {
					name: "______"
				},
				HTMLclass: "nodo_invisible",
				HTMLid: "rama_" + rama.id
		};
				
		let nuevoNodoInvisible = treant.tree.addNode(mapNodos.get(rama.padre), nodeInvisible);
		
		mapRamasDisponibles.set(rama.id, nuevoNodoInvisible);
	}
}

function graficarNodos(nodos){
	for(let i in nodos){
		let nodo = nodos[i];
		
		let nodoInvisible = mapRamasDisponibles.get(nodo.idRama);
		
		nodoInvisible.nodeHTMLclass = "";
		nodoInvisible.nodeHTMLid = "nodo_" + nodo.id;

		nodoInvisible.nodeDOM.id = "nodo_" + nodo.id;
		$(nodoInvisible.nodeDOM).removeClass('nodo_invisible');
		/*
		 * La siguiente instrucción se debe a que el quitarle la clase 'nodo_invisible' puede modificar la altura del 'div'
		 * (si se agregase borde al remover esta clase, por ejemplo).
		 * El hecho de que se llame antes de setTextName, es que sabemos que este método ejecutará un redraw(), por lo tanto evitamos
		 * ejecutarlos nosotros para actualizar el cambio en la altura.
		 */
		
		nodoInvisible.height = nodoInvisible.nodeDOM.offsetHeight+1;
		nodoInvisible.setTextName(nodo.rotulo);
		
		mapRamasDisponibles.delete(nodo.idRama);
		mapNodos.set(nodo.id, nodoInvisible);
	}
}

function graficarRamasCut(ramas){
	for(let i in ramas){
		let rama = ramas[i];
		
		mapRamasDisponibles.get(rama.id).cutRama();
		mapRamasDisponibles.delete(rama.id);
	}
}

function graficarSustituciones(susts){
	for(let i in susts){
		var sust = susts[i];
		
		var idNode_inTree = mapNodos.get(sust.idNodo).id;
		var rama = treant.tree.connectionStore[idNode_inTree];

		rama.attr('cursor', 'pointer');
		rama.attr('title', sust.sustitution);

		rama.click(() => {
			$('#subIndiceSust').text(sust.id);
			$('#sustitucionText').text(sust.sustitution);
			$('#sustitucionModel').modal('show');
		});

		rama.hover(() => {
			rama.attr('stroke-width', 3);
		}, () => {
			rama.attr('stroke-width', 2);
		});
	}
}

export function hayAnimacionesEnCurso(){
    return treant.tree.animacionesEnCurso == 0;
}



window.onbeforeunload = function(event) {
	if(myID){
		$.post('eliminarArbol', 'id='+myID);
		
		event.returnValue = "";
	}
}