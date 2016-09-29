#!/bin/bash
./rest-backend -c rest-backend-config.js &
echo $! > rest-backend-running.pid
