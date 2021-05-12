#!/usr/bin/env bash

echo '# Run ".create-gitignore.sh > .gitignore" to recreate

### Custom ###

.idea
.vscode'

# Scala/SBT
curl -sL 'https://www.gitignore.io/api/scala,sbt,bloop,metals'

# Editors
curl -sL 'https://www.gitignore.io/api/intellij,visualstudiocode,sublimetext'
