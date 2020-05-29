package com.gmail.francozanardi97.app.domain;

import com.gmail.francozanardi97.app.dto.ProgramaUsuario;

public class NotificacionError {
	private String descripcion;
	private ProgramaUsuario programaUsuario;
	
	public NotificacionError(ProgramaUsuario pu, String desc) {
		programaUsuario = new ProgramaUsuario();
		programaUsuario.setQueryProlog(pu.getQueryProlog());
		programaUsuario.setSourceCode(pu.getSourceCode());
		descripcion = desc;
	}
	
	
	public String getDescripcion() {
		return descripcion;
	}
	public void setDescripcion(String descripcion) {
		this.descripcion = descripcion;
	}
	public ProgramaUsuario getProgramaUsuario() {
		return programaUsuario;
	}
	public void setProgramaUsuario(ProgramaUsuario programaUsuario) {
		this.programaUsuario = programaUsuario;
	}
	
	
}
