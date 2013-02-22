#!/bin/bash
for i in $(git tag); do (echo $i | grep -q / - ); if test $? == 1; then git tag -d $i; fi; done

