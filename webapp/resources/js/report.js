import * as alerts from './alerts.js';

$("#reportSend").click(() => {
	$.post('notificarError', 'id='+myID+"&descripcion_error="+encodeURIComponent($("#reportDescription").val()))
	.done(() => {
		alerts.showAlertSuccess('¡Error enviado!', 'Gracias por reportar el error.');
	}).fail(() => {
		alerts.showAlertError('¡Se ha producido un fallo interno!', 'Inténtelo más tarde.');
	}).always(() => {
		$('#reportModal').modal('hide');
		$('#reportDescription').val("");
		
	})

});