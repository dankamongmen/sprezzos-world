#!/bin/sh
#
# junit command line program for Debian
# author:  Takashi Okamoto <tora@debian.org>
# usage:
#        junit [-text] <TestCase> output result with text. 
#                                 This mode is default.
#        junit -awt <TestCase>    output result with awt ui.
#        junit -swing <TestCase>  output result with swing ui.


TESTRUNNER=junit.textui.TestRunner
CLASSPATH=${CLASSPATH}:/usr/share/java/junit.jar:.

if [ "$#" = "0" ]; then
  TESTRUNNER=junit.awtui.TestRunner
fi

if [ "$1" = "-text" ] ; then
  shift;
  if [ "$#" = "0" ] ; then
    FLAG=false
  fi
elif [ "$1" = "-swing" ] ; then
  shift;
  TESTRUNNER=junit.swingui.TestRunner
  if [ "$#" != "0" ]; then
	echo "-swing option should not have other arguments"
	exit;
  fi

elif [ "$1" = "-awt" ] ; then
  shift
  TESTRUNNER=junit.awtui.TestRunner
  if [ "$#" != "0" ]; then	
	echo "-awt option should not have other arguments"
	exit;
  fi

fi


if [ "$1" = "-help" ] || [ "$FLAG" = "false" ] ; then
  echo "junit 3.8.1 -- this version is modified by Takashi Okamoto <tora@debian.org> for Debian."
  echo "Usage:	junit "
  echo "		-text  <TestCaseName> -  using text user interface."
  echo "		-awt	- using awt user interface."
  echo "		-swing	- using swing user interface."
  echo "TestCaseName is the name of the TestCase class"
  exit
fi

exec java -classpath ${CLASSPATH} ${TESTRUNNER}  ${1+"$@"}



