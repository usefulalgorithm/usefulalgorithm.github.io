---
title: Create New Posts by Making Pull Requests
layout: post
comments: false
tags: about this blog
---

This is a little easier than I thought it would be. So basically it generates a post from a pull request's body and commits it to  branch. I already have an action that reruns the site generator whenever something new appears in  directory, so in effect I just need to write whatever I want to write in the PR's body, and press  when I'm happy with it.

More specifically there are 4 GitHub actions in action (no pun intended):

1. Creates a PR by running an action: I want to be able to do it very easily, with this I only need to type in the title of the post and a new PR will be populated. Note that in order to make a new PR, I need to insert something into the new commit. That something is a file named .
2. Generates the new post file in  directory: when I'm good with the PR body and merge the PR, an action will pipe the body into the new file. The draft iteme will also be removed.
3. Run the Haskell site generator. The action detects if anything's changed in , and if so generates the static site. The generator itself doesn't change much, the action nicely takes care of caching it so that it doesn't spend twenty minutes compiling hakyll.
4. Deploy to GH pages. I didn't write this myself.
