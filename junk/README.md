# `junk`

Code that I put a reasonable amount of work into writing but then discovered that either (A) it wasn't a good enough approach for my scenario or (B) I didn't need it after all.  It also might be the case that (C) the code WAS an integral part of my code base until my research moved onto other things.

Yes, of course, git remembers all, and it is a cardinal sin to leave old, unused source code dangling about in tree on `master`.  So why does this directory exist? Two reasons:

**One: Potential reuse.** So that, in the future, if I ever happen to be grepping about my home directory for "that thing I remember writing a while back," then I'll have a chance of locating it with a simple `rg` (as opposed to `rg -uuu`).

**Two: To learn from my mistakes.** I do have a terrible knack for pre-emptively writing abstractions. Most of my unused code is the direct result of trying to solve the wrong problem.  Some of these files are documented with post-mortems to remind future-me about why they were terrible ideas.
