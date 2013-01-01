set -e
mkdir _test
(cd _test ; cmake .. -DINTERNAL_WIBBLE=ON)
(cd _test ; make )
(cd _test ; make check )
