---
title: AstonHack and DrumHub
tags: webaudio, github
published: false
---

A couple of weeks ago I went along to AstonHack 2016 as part of the Majestic
team.  We were mainly there to support the students trying to use Majestic
API, but were pretty happy to assist them with whatever tech they were having
trouble with. It was reasonably busy at first, but soon I found myself thinking
I should probably start working on one of my ideas since I'm here anyway and
everybody seems to be busy with their hacks.

Since listening to a talk about WebAudio API during Hackference 2016 I've had
this idea of taking a git repository and using the commit timings to create a
drum track. I wanted the rythm to speed up when the commit frequency was high
and vice versa. Instead of trying to figure out the commit frequency I decided
that every period between two commits would become one 4/4 beat. The other
feature I wanted to add is somehow distinguishing the notes that fall on BPM
peaks and troffs (e.g. if the rythm is slowing down and after a certain point
starts picking up I want the note at that point to be different). I ended up
picking a crash cymbal to denote the peaks and a tom-tom drum for the troffs.

Luckily for me GitHub has a [REST API for their repositories](https://developer.github.com/v3/), so I didn't
need to do any git parsing myself. It also meant I could build the application
without any server-side code and host it using [GitHub Pages](https://pages.github.com/). It turns out I could
get all the data using one request to the API (asking for a list of commits). The
only thing left to do there was transforming the data into a time series. This
was the first challenge as GitHub gives two timestamps - one as the "commiter"
timestamp and one as the "author" one. This difference emerges from the fact
that the original author of the commit might not have write access to the repository,
in which case someone (the commiter) can take their commits and apply them.
However, when multiple commits are applied this way the commiter timestamp of
these commits is identical between the commits even if the author commits
have different timestamps.

The commits are returned ordered by the commiter timestamp (as this is the
order they appear in the repository), however, because of the duplication of
timestamps it didn't really fit my purpose. Of course, I could've removed the
duplicate timestamps, but I felt that wouldn't have been the best
representation of the activity on the repository. Instead, I decided I'd use
the author timestamps and simply sort them, which worked really well.


