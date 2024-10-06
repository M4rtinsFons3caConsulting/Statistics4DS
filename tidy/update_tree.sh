#!/bin/bash
# This script generates a project tree for a given directory and saves it to project_tree.txt

cd ~/MsC/Statistics4DS/
tree -a -I ".git" > project_tree.txt
