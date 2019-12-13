package com.gmail.francozanardi97.app.service;

import java.sql.SQLException;

import com.gmail.francozanardi97.app.domain.NotificacionError;

public interface ServiceNotificacionError {
	void guardarNotificacion(NotificacionError not) throws SQLException;
}
