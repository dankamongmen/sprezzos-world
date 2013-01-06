#!/bin/sh

# Script to automate running the provided example programs to test all
# language binding (cpp, python, java)

# Needs to be run from the directory containing the newly-built deb's

set -e

test -f protobuf-compiler_*.deb || \
    { echo "Cannot find protobuf-compiler deb file, exiting" 1>&2; exit 1; }

echo "*** Installing dependencies (hardcoded list)"

apt-get install -y openjdk-6-jdk python python-support pkg-config zlib1g-dev

echo "*** Installing packages"

dpkg -i *.deb

cd /usr/share/doc/protobuf-compiler/examples

export CLASSPATH=/usr/share/java/protobuf.jar

echo "*** Building example programs"

make

TEST_FILE=`mktemp`
echo "Test file is $TEST_FILE"

echo "*** Adding via cpp"
./add_person_cpp $TEST_FILE << EOF
1
Person One


EOF

echo "*** Adding via python"

./add_person_python $TEST_FILE << EOF
2
Person Two
two@example.net

EOF

echo "*** Adding via java"

./add_person_java $TEST_FILE << EOF
2
Person Two

555-1234
work

EOF



for kind in cpp python java; do
  echo "*** Listing via $kind"
  ./list_people_$kind $TEST_FILE
done

rm $TEST_FILE
