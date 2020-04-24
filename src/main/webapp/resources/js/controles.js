import * as graficador from './graficador.js';
import * as alerts from './alerts.js';

$('#statusButton').click(() => {
	var ultimoNodo = graficador.getUltimoNodo();

	var status = "Running";

	if(graficador.esNodoSolucion(ultimoNodo)){
		status = "Waiting";
	}
	
	if(graficador.arbolFinalizado(ultimoNodo)){
		status = "Finished";
	}

	$('#estadoArbol').text(status);
	$('#cantidadNodos').text(graficador.getCantidadNodos());
	$('#cantidadSoluciones').text(graficador.getCantidadSoluciones());

	$('#statusModal').modal('show');

	
});

$('#stopButton').click(() => {
	graficador.eliminarArbol(() => {
        document.getElementById("createButton").disabled = false;
		document.getElementById("createButton").style.display = "block";
		document.getElementById("nextButton").style.display = "none";
		document.getElementById("stopButton").style.display = "none";

		document.getElementById("nextStepButton").style.display = "none";
		document.getElementById("skipButton").style.display = "none";
		document.getElementById("skipAllButton").style.display = "none";
		document.getElementById("statusButton").style.display = "none";
    })
});

$('#nextButton').click(() => {
	$('#nextButton').attr("disabled", true);
    graficador.quitarAnimacion();

	graficador.nextFotograma((nodos) => {
        graficador.restoreSpeedAnimation();

		actualizarControles(nodos[0]);

	});
	
	
});

$('#skipButton').click(() => {
	graficador.quitarAnimacion();

	graficador.nextFotograma((nodos) => {
		graficador.restoreSpeedAnimation();

		if(nodos.length == 1){
			actualizarControles(nodos[0]);
		}

	});

});

$('#skipAllButton').click(() => {
	graficador.quitarAnimacion();

	desactivarControlesNext();
	$('#statusButton').attr("disabled", true);
	
	finalizarArbol((ultimoNodo) => {
		graficador.restoreSpeedAnimation();

		actualizarControles(ultimoNodo);
		$('#statusButton').attr("disabled", false);
	});

});

$('#nextStepButton').click(() => {
	desactivarControlesNext();

	graficador.nextFotograma((nodos) => {
		var interval = setInterval(function() {

			if(graficador.hayAnimacionesEnCurso()){
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
           
            document.getElementById("createButton").disabled = true;
            
            graficador.crearArbol(() => {
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
            }, 
            (error) => {
                alerts.showAlertError('¡Se ha producido un fallo interno!', error.responseText.length < 60 ? error.responseText : (error.responseText.substring(0, 60) + '...'));
                document.getElementById("createButton").disabled = false;
            });

			evento.preventDefault();
		}
);

function finalizarArbol(callback){
	graficador.nextFotograma((nodos) => {
		if(nodos.length === 1){
			if(!graficador.esNodoSolucion(nodos[0]) && !graficador.arbolFinalizado(nodos[0])){
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
	if(graficador.esNodoSolucion(ultimoNodo)){
		desactivarControlesNext();
		graficador.agregarSolucion(ultimoNodo);

		if(!graficador.arbolFinalizado(ultimoNodo)){
			$('#nextButton').attr("disabled", false);
		} 
	} else if(graficador.arbolFinalizado(ultimoNodo)){
		desactivarControlesNext();
	} else {
		activarControlesNext();
	}
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

