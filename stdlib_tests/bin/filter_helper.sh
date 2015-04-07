#!/bin/sh
exec sed -e 's/# ir: //;q' "$1"
