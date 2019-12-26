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

var editor = CodeMirror.fromTextArea(document.getElementById("sourceCode"), {
	lineNumbers : true,
	matchBrackets : true,
	theme : "prolog"
});



window.onbeforeunload = function(event) {
	if(myID){
		$.post('eliminarArbol', 'id='+myID);
		
		event.returnValue = "";
	}
}

$('#statusButton').click(() => {
	var ultimoNodo = Array.from(mapNodos.values()).pop();
	ultimoNodo = {rotulo: ultimoNodo.text.name};

	var status = "Running";

	if(esNodoSolucion(ultimoNodo)){
		status = "Waiting";

		if(arbolFinalizado(ultimoNodo)){
			status = "Finished";
		}
	}

	$('#estadoArbol').text(status);
	$('#cantidadNodos').text(mapNodos.size);
	$('#cantidadSoluciones').text(cantSoluciones);

	$('#statusModal').modal('show');

	
});

$('#stopButton').click(() => {
	$.post('eliminarArbol', 'id='+myID)
	.always(() => {
		document.getElementById("createButton").disabled = false;
		document.getElementById("createButton").style.display = "block";
		document.getElementById("nextButton").style.display = "none";
		document.getElementById("stopButton").style.display = "none";

		document.getElementById("nextStepButton").style.display = "none";
		document.getElementById("skipButton").style.display = "none";
		document.getElementById("skipAllButton").style.display = "none";
		document.getElementById("statusButton").style.display = "none";
		
		mapRamasDisponibles.clear();
		mapNodos.clear();
		cantSoluciones = 0;
		
		myID = null;

		configTreeDefault.nodeStructure = {HTMLclass: "nodo_invisible"};
		
		treant.tree.reload();
		configTreeDefault.nodeStructure = {};
	});
});

$('#nextButton').click(() => {
	var nodoAgregado;
	$('#nextButton').attr("disabled", true);
	treant.tree.CONFIG.animation.connectorsSpeed = 0;
	treant.tree.CONFIG.animation.nodeSpeed = 0;

	nextFotograma((nodos) => {
		treant.tree.CONFIG.animation.connectorsSpeed = SPEED_ANIMATION;
		treant.tree.CONFIG.animation.nodeSpeed = SPEED_ANIMATION;

		actualizarControles(nodos[0]);

	});
	
	
});

$('#skipButton').click(() => {
	treant.tree.CONFIG.animation.connectorsSpeed = 0;
	treant.tree.CONFIG.animation.nodeSpeed = 0;

	nextFotograma((nodos) => {
		treant.tree.CONFIG.animation.connectorsSpeed = SPEED_ANIMATION;
		treant.tree.CONFIG.animation.nodeSpeed = SPEED_ANIMATION;

		if(nodos.length == 1){
			actualizarControles(nodos[0]);
		}

	});

});

$('#skipAllButton').click(() => {
	treant.tree.CONFIG.animation.connectorsSpeed = 0;
	treant.tree.CONFIG.animation.nodeSpeed = 0;

	desactivarControlesNext();
	$('#statusButton').attr("disabled", true);
	
	finalizarArbol((ultimoNodo) => {
		treant.tree.CONFIG.animation.connectorsSpeed = SPEED_ANIMATION;
		treant.tree.CONFIG.animation.nodeSpeed = SPEED_ANIMATION;

		actualizarControles(ultimoNodo);
		$('#statusButton').attr("disabled", false);
	});

});

$('#nextStepButton').click(() => {
	desactivarControlesNext();

	nextFotograma((nodos) => {
		var interval = setInterval(function() {

			if(treant.tree.animacionesEnCurso == 0){
				if(nodos.length == 1){
					actualizarControles(nodos[0]);
				}
				
				clearInterval(interval);
			}


		}, 100);
	});
});


$('#program').submit(
		function(evento) {

			var s = "sourceCode=" + encodeURIComponent(editor.getValue()) + "&queryProlog="
					+ encodeURIComponent($('#queryProlog').val());

			document.getElementById("createButton").disabled = true;

			$.post('', s)
				.then(id => {
					myID = id;
					
					return $.post('getRaiz', 'id='+myID);
				})
				.then(raiz => {
					crearArbol(raiz);

					document.getElementById("createButton").style.display = "none";
					document.getElementById("nextButton").style.display = "block";
					document.getElementById("stopButton").style.display = "block";
		
					document.getElementById("nextStepButton").style.display = "block";
					document.getElementById("skipButton").style.display = "block";
					document.getElementById("skipAllButton").style.display = "block";
					document.getElementById("statusButton").style.display = "block";
					
					document.getElementById("nextButton").disabled = true;
					document.getElementById("nextStepButton").disabled = false;
					document.getElementById("skipButton").disabled = false;
					document.getElementById("skipAllButton").disabled = false;
					document.getElementById("statusButton").disabled = false;


				})
				.catch((error) => {
					console.log('error: ', error);
					showAlertError('¡Se ha producido un fallo interno!', error.responseText.length < 60 ? error.responseText : (error.responseText.substring(0, 60) + '...'));
					document.getElementById("createButton").disabled = false;
				})

			evento.preventDefault();
		}
);

function finalizarArbol(callback){
	nextFotograma((nodos) => {
		if(nodos.length === 1){
			if(!esNodoSolucion(nodos[0]) && !arbolFinalizado(nodos[0])){
				finalizarArbol(callback);
			} else {
				if(callback && typeof(callback) === "function"){
					callback(nodos[0]);
				}
			}
		} else {
			finalizarArbol(callback);
		}
	});
}

function actualizarControles(ultimoNodo){
	if(esNodoSolucion(ultimoNodo)){
		desactivarControlesNext();
		agregarSolucion(ultimoNodo);

		if(!arbolFinalizado(ultimoNodo)){
			$('#nextButton').attr("disabled", false);
		} 
	} else if(arbolFinalizado(ultimoNodo)){
		desactivarControlesNext();
	} else {
		activarControlesNext();
	}
}

function esNodoSolucion(nodo){
	return nodo.rotulo === "[]";
}

function arbolFinalizado(ultimoNodo){
	return (esNodoSolucion(ultimoNodo) || ultimoNodo.rotulo === "[fail]") && mapRamasDisponibles.size === 0;
}

function agregarSolucion(nodo){
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


function nextFotograma(callback){
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

function crearArbol(nodo){
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



function configButtons_fullScreen(){
	
	//si todos son nulls entonces no está en fullscreen.
	function isFullScreenOn() {
		return document.fullScreenElement || document.webkitFullscreenElement || document.msFullscreenElement || document.mozFullScreenElement;
	}
	
	function acomodarTree() {
		setTimeout(function() {
			treant.tree.redraw();
		}, 500);
	}
	
	function configExitFullScreen(){
		
		function exitFullScreen() {
			if(!isFullScreenOn()){ 
				btnFullscreen.style.cssText = "display: block;";
				btnreport.style.cssText = "display: block;";
				viewTree.style.cssText = "height: 100%!important;";
				acomodarTree();
			}
		} 
		
		/* Standard syntax */
		document.addEventListener("fullscreenchange", exitFullScreen);
		/* Firefox */
		document.addEventListener("mozfullscreenchange", exitFullScreen);
		/* Chrome, Safari and Opera */
		document.addEventListener("webkitfullscreenchange",  exitFullScreen);
		/* IE / Edge */
		document.addEventListener("msfullscreenchange",  exitFullScreen);
		
	}
	
	function addListnerToFullscreen(){
		btnFullscreen.addEventListener("click", () => {
			btnFullscreen.style.cssText = "display: none;";
			btnreport.style.cssText = "display: none;";
			viewTree.style.cssText = "height: 90%!important;";

			var controlsAndView = document.getElementById('controlsAndViewTree');
			
			if (controlsAndView.requestFullscreen) {
				controlsAndView.requestFullscreen();
			} else if (controlsAndView.mozRequestFullScreen) { /* Firefox */
				controlsAndView.mozRequestFullScreen();
			} else if (controlsAndView.webkitRequestFullscreen) { /* Chrome, Safari & Opera */
				controlsAndView.webkitRequestFullscreen();
			} else if (controlsAndView.msRequestFullscreen) { /* IE/Edge */
				controlsAndView.msRequestFullscreen();
			}
			
			acomodarTree();
		});
	}

	function configHover(){
		$(viewTree).hover(() => {
			if(!isFullScreenOn() && mapNodos.size > 0){
				btnFullscreen.style.cssText = "display: block;";
				btnreport.style.cssText = "display: block;";
			}
		}, () => {
			btnFullscreen.style.cssText = "display: none;";
			btnreport.style.cssText = "display: none;";
		});
	}
	
	var btnFullscreen, btnreport, viewTree;

	btnFullscreen = document.getElementById('fullScreenButton');
	btnreport = document.getElementById('reportButton');
	viewTree = document.getElementById('viewTree');

	
	configHover();
	addListnerToFullscreen();
	configExitFullScreen();	
}

function activarControlesNext(){
	$('#nextStepButton').attr("disabled", false);
	$('#skipButton').attr("disabled", false);
	$('#skipAllButton').attr("disabled", false);
}

function desactivarControlesNext(){
	$('#nextStepButton').attr("disabled", true);
	$('#skipButton').attr("disabled", true);
	$('#skipAllButton').attr("disabled", true);
}

$("#reportSend").click(() => {
	$.post('notificarError', 'id='+myID+"&descripcion_error="+encodeURIComponent($("#reportDescription").val()))
	.done(() => {
		showAlertSuccess('¡Error enviado!', 'Gracias por reportar el error.');
	}).fail(() => {
		showAlertError('¡Se ha producido un fallo interno!', 'Inténtelo más tarde.');
	}).always(() => {
		$('#reportModal').modal('hide');
		$('#reportDescription').val("");
		
	})

});

$("#closeAlert").click(() => {
	$("#alert").hide();
})

function showAlertError(title, body){
	$('#alert').removeClass('alert-success');
	$('#alert').addClass('alert-danger');
	$('#alert span').html('<strong>' + title + '</strong> ' + body);
	$('#alert').show();
}

function showAlertSuccess(title, body){
	$('#alert').removeClass('alert-danger');
	$('#alert').addClass('alert-success');
	$('#alert span').html('<strong>' + title + '</strong> ' + body);
	$('#alert').show();
}


configButtons_fullScreen(); //esto no tendría que crearlo cada vez que creo un árbol
//además cuando hago stop tendría que desactivar la función del hover en el div sldtree.

//t.tree.addNode(nodoPadre, nuevoNodo);

/*
 * obtener ubicacion de rama -> sea N el nodo hijo de la rama:
 * M estará en la posición: n.parent().connectorPoint(true);
 * L estará en la posición: n.connectorPoint(false);
 * 
 * Siendo M y L elemento del path en SVG html.

*/

// t.tree.connectionStore[3].attr("stroke", "red"); permite cambiar el color de una rama a rojo. 
// Esto lo podemos usar a la hora de hacer un cut a una rama y querer cambiar su color a gris.

/*
 * Formas de hacer un nodo visible:
 * 	- Sabiendo su HTMLid, lo buscamos en el DOM y seteamos className.
 *  - Guardando una referencia al TreeNode, de esta manera si n fuera el TreeNode haríamos:
 *  	n.nodeDOM.className = ...
 *  
 *  Para cambiar el texto necesitamos una referencia al nodo, o nuevamente un id para buscarlo en el tree.
 *  En este último caso usariamos: t.tree.nodeDB.db[id], si el id es igual al indice del arreglo o buscaríamos elemento a elemento.
 * 
 *  Para agregar o remover las css clases mejor usar jquery:  $(this).addClass('className'),  $(this).removeClass('className');
 * 
*/
