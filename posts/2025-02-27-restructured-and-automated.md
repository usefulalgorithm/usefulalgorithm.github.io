---
title: Restructured and Added Some Automation for This Blog
layout: post
comments: false
tags: about this blog
---

I have been avoiding changing things up in this blog even though I really should have done this a long time ago. There were several things I did not like so much about it:

### Having to Have a Working Haskell Environment

To actually generate the site, I needed to do the following:

- `stack build` - this builds the executable called `site` that consumes content in `posts/` and churns out HTML files.
- `stack exec site build` - this runs the `site` executable and builds the HTML files, which are stored in `_site/`.

Since everything is built offline, a working Haskell environment was necessary.

### Lots of Manual Steps

As stated above, since the generated HTML files are in `_site/`, I needed to find a way to get GH pages to host them. The way I did it was to have [another separate repo](https://github.com/usefulalgorithm/old-website), make sure `_site/` is pointing to that repo, then run `stack exec site build`, check that things are generated correctly in `_site`, and finally push to both this repo and the actual GH pages repo.

---

Now that I'm not employed and have some free time, I decided it was finally time to make it more usable and generally encourage myself to post more often. Here's what I did:

### Deprecate the Old GH Pages Repo and Just Use This One

This should be quite obvious - there's a [tutorial](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html) that tells you how to change your executable's target directory from `_site/` to `docs/`. I can just ditch the old repo, rename the Haskell repo to `usefulalgorithm.github.io`, and have its page deployed from `docs/` in the main branch. I'm pretty sure the directory has to be called `docs/` and not anything else though.

### Build & Deploy from GH Actions

I've done a bunch of GH actions in my previous job and found them to be a huge time saver. So what I wanted to do is just update Markdown files in `posts/`, and then trigger the Haskell commands from within the action runner.

---

Some things I hope to do in the near future. These aren't hard in themselves but probably require a little bit more consideration.

### Post via PRs

Writing Markdown files is still a little annoying, especially when I'm on my phone and just want to post something to my blog. I want to find a way to create posts through pull requests, but I need to think about where to put things like tags and how to format the pull request message into a proper Markdown file.

### Repost to Threads (and Possibly Other Platforms)

More often than not, I would repost the published post to my socials. I know social media is like the worst thing that's happened in 20 years, but I still want people to read what I have to say.
