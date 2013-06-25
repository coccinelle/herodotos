#!/bin/bash

if [ -z "$DBNAME" ]; then
		DBNAME=stable-2.6.32
fi

if [ -z "$LINUXES" ]; then
		LINUXES=/var/linuxes/stable
fi

if [ -z "$VERSION_FILE" ]; then
		VERSION_FILE=versions.txt
fi

