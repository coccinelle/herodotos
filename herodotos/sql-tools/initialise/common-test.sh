#!/bin/bash

if [ -z "$DBNAME" ]; then
		DBNAME=demo
fi

if [ -z "$LINUXES" ]; then
		LINUXES=/home/manseur/demo/test
fi

if [ -z "$VERSION_FILE" ]; then
		VERSION_FILE=versions-test.txt
fi

