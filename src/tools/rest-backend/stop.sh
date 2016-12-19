#!/bin/sh
kill -TERM $(cat /run/elektra-@tool@.pid)
rm /run/elektra-@tool@.pid
