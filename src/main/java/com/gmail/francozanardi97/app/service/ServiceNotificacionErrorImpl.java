package com.gmail.francozanardi97.app.service;

import java.sql.SQLException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gmail.francozanardi97.app.dao.NotificacionErrorDAO;
import com.gmail.francozanardi97.app.domain.NotificacionError;

@Service
public class ServiceNotificacionErrorImpl implements ServiceNotificacionError {

	@Autowired
	private NotificacionErrorDAO notifDao;
	
	@Override
	public void guardarNotificacion(NotificacionError not) throws SQLException {
		notifDao.save(not);
	}

}
