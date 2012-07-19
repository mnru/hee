#!/bin/bash
java -Xmx512M -Dfile.encoding=UTF-8 -jar `dirname $0`/sbt-launch.jar "$@"
