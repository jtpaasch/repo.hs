Repo.hs
=======

A very simple implementation of git, written in haskell. I wrote this for a presentation on how git works. It is for educational purposes -- it is meant to show how _git_ works, not how _haskell_ works (real haskellers will likely gasp in horror at the haskell here). 

I chose to write this in haskell simply because the presentation's audience was diverse, and when written in a certain way, haskell can be very readable for people who may or may not have any expertise in writing much code. 

I may upload the presentation's slides someday, after I clean them up a little.

Compiling
---------

The only dependency is cryptohash. 

Make sure you have virthualenv installed, create a virtual environment, put repo.hs in that folder, then install cryptohash with cabal:

    > cabal install cryptohash

It should then compile fine with:

    > ghc --make repo

You can then install it for use system wide with:

    > sudo -u root install repo /usr/local/bin

Usage
-----

The important thing is to read the source code (for learning purposes). This is not meant for practical use! But, the commands go as follows.

To create a new repository in a directory, cd into that directory, then do this:

    > repo init

To see the status of your work:

    > repo status

To add a file to the "stage":

    > repo stage file.txt

To remove a file from the "stage":

    > repo unstage file.txt

To commit the stage:

    > repo commit

To see all branches:

    > repo branches

To create a branch:

    > repo branch new-branch

To switch to a new branch:

    > repo checkout new-branch

To merge two branches:

    > repo merge other-branch

Caveats
-----------------

Again, this is for educational purposes. So, note these caveats:

* `repo` only does fast forward merges. It cannot merge anything more complex, and will ask you to do it manually in such cases.
* `repo` does not distribute across many directories or computers. It only works locally, in one folder. Maybe someday I'll build a distributed version, but for now, this is just meant to illustrate how git works locally. 
* `repo` has bugs!
