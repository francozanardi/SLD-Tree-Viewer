$("#closeAlert").click(() => {
	$("#alert").hide();
})

export function showAlertError(title, body){
	$('#alert').removeClass('alert-success');
	$('#alert').addClass('alert-danger');
	$('#alert span').html('<strong>' + title + '</strong> ' + body);
	$('#alert').show();
}

export function showAlertSuccess(title, body){
	$('#alert').removeClass('alert-danger');
	$('#alert').addClass('alert-success');
	$('#alert span').html('<strong>' + title + '</strong> ' + body);
	$('#alert').show();
}