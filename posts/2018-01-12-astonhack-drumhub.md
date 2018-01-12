title: AstonHack 2016 and DrumHub
tags: webaudio, github
published: 2018-01-12

More than a year ago I went along to AstonHack 2016 as part of the Majestic
team.  We were mainly there to support the students trying to use Majestic
API, but were pretty happy to assist them with whatever tech they were having
trouble with. It was reasonably busy at first, but soon I found myself thinking
I should probably start working on one of my ideas since I'm there anyway and
everybody seemed to be busy with their hacks.

Since listening to a talk about WebAudio API during Hackference 2016 I've had
this idea of taking a git repository and using the commit timings to create a
drum track. I wanted the rhythm to speed up when the commit frequency was high
and vice versa. Instead of trying to figure out the commit frequency I decided
that every period between two commits would become one 4/4 measure and I would
vary the tempo according to period between two adjacent commits. The other
feature I wanted to add is somehow distinguishing the notes that fall on BPM
peaks and troffs (e.g.  if the rhythm is slowing down and after a certain point
starts picking up I want the sample at that point to be different). I ended up
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

The GitHub API returns commits ordered by the commiter timestamp (as this is the
order they appear in the repository). However, because of the duplication of
timestamps it didn't really fit my purpose. Of course, I could've removed the
duplicate timestamps, but I felt that wouldn't have been the best
representation of the activity on the repository. Instead, I decided I'd use
the author timestamps and simply sort them, which worked really well.

I knew that I wanted to map the period between commits into reasonable BPM as
otherwise playing one repository could take months if not years. The mapping
imposed a limit on the maximum BPM so that the shortest period between commits
in any repository would correspond to one measure played at 480 beats per minute.
When commits were spaced out more the tempo would decay exponentially.
While testing I realised that many repositories are updated quite sporadically
and in bursts - there were long periods of very low freaquency, so I added a limit
to the lowest tempo and chose it to be slower by no more than a factor of 10. The
resulting function for scaling is then

![BPM Scaling Formula](/img/astonhack-drumhub/bpm-formula.png)

where <var>A</var> is the smallest time period used for one measure (4 beats at
480 BPM or 0.5 s), <var>v</var> is the factor limiting the lowest tempo
(in this case 10), <var>t<sub>min</sub></var> is the smallest period between
commits and <var>t</var> is the period between the current commit and the
previous one.

One value that was quite difficult to get right in the above formula was the
value of <var>k</var> - the constant that controls the exponential decay or
in our case how easily the output BPM drops to the lowest value. After a good
amount of experimentation and listening to what different repositories sounded
like I settled to a value of <var>k</var> = 0.00001 s<sup>-1</sup>.

Once I had the commit data and scaled it appropriately the only thing left was
to actually schedule the samples and press play. I used
[this intro to Web Audio API](https://www.html5rocks.com/en/tutorials/audio/scheduling://www.html5rocks.com/en/tutorials/webaudio/intro/)
by Boris Smus to figure out how to get the browser to do what I wanted. I also
threw together a simple UI using [Bootstrap](https://getbootstrap.com/) and a
photo from [Unsplash](https://unsplash.com/).

You can find the hosted result at [virtual.gq/drumhub](http://virtual.gq/drumhub)
and the code at the [GitHub repo](https://github.com/gkudelis/drumhub).
Pick a GitHub repository, put the username/reponame in the text field and press
play.  If you're not sure what repository to try I suggest starting with
tensorflow/tensorflow.
