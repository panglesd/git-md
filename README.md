# Git-md

A tool to locally synchronize your hackmd notes. Experimental: use at your own risk, but report bugs!

## Quick-start

1. Generate a Hackmd token, `<token>`
2. Create a directory (and make it readable by the docker container)

```shell
$ mkdir .git_md
$ chmod a+rw .git_md   # Could be more specific to the docker container
```

3. Start the docker image,

```shell
$ docker run --rm -v ~/.git_md:/home/git -p 2222:22 panglesd/git_md
```

3.5. Ssh into the docker container to set up some things ...

```shell
$ ssh git@localhost -p 2222
$ git config --global user.email "you@example.com"
$ git config --global user.name "Your Name"
$ exit
```

`

4. Clone your hackmd, for instance into the `hackmd` directory

```shell
$ git clone ssh://git@localhost:2222/home/git/<token> hackmd
```

Now, each `git pull` will pull your notes from hackmd (as a single commit) and each `push` will update the notes (if your local repo is up to date)

## More information

`git_md` can work on two setup:
- local modes, where `git-md pull -t <token>` and `git-md push -t <token>` will pull and push your notes back and forth your computer and the hackmd instance. Uses timestamps to not send and receive too many notes. Use with care, conflicts are not detected at all!
- git mode, with `git-md upload-pack` and `git-md receive-pack` replacing the server-side git commands, to have a git server as a gateway to your hackmd notes.

The docker image is a simple ssh server where the `git upload-pack` and `git receive-pack` have been replaced by their `git-md` counterpart.

TODO: explain how it works under the hood.
