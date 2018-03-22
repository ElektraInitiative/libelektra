#!/bin/sh
kill -TERM $(cat /run/elektra-@tool@.pid)
kill -TERM $(cat /run/elektra-@tool@-elektrad.pid)
