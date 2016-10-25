#!/bin/bash
./rest-backend -c rest-backend-config.json &
echo $! > rest-backend-running.pid
