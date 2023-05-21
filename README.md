# Git-md

A tool to locally synchronize your hackmd notes

Works on two setup:
- local modes, where `git-md pull -t <token>` and `git-md push -t <token>` will pull and push your notes back and forth your computer and the hackmd instance
- git mode, with `git-md upload-pack` and `git-md receive-pack` replacing the server-side git commands, to have a git server as a gateway to your hackmd notes
