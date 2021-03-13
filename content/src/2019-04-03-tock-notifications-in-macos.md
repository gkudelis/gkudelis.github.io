+++
title = "Tock notifications in macOS"
[taxonomies]
tags = ["other"]
+++

Quite a while ago I've come across the concept of tocks on the
[Beeminder Blog](https://blog.beeminder.com/tocks/) and started using them when
I work. The general idea is that you work in 45 minute uninterrupted chunks and
count the number of these you do in a day. I've noticed it works well for me
and stuck with it.

I was going about it in quite a lazy way tool-wise - I'd simply find a timer
that would run in a browser tab, set it to 45 minutes and use that. However,
the only way for it to notify me is if I set an audible alarm. Recently I
found myself at a library without headphones and recognizing not everyone might
appreciate the loud alarm going off.

I'm on a Mac, so an alert that applications can show in the upper right corner
seemed like a natural solution - I looked for a CLI tool to display alerts and
found [terminal-notifier](https://github.com/julienXX/terminal-notifier). It
works great, but it shows a message immediately when you call it and I needed a
timeout. I also quite like to be able to check how much time I have left in the
current tock and be able to pause it when needed, so it would be nice to do
something more clever than `sleep 2700`. A great solution is using another tool
called [termdown](https://github.com/trehn/termdown), which displays a nice
ASCII art countdown in your terminal window and lets you pause it using the
space key. You can also add a suffix to signify hours or minutes. I am calling
it as
```sh
termdown 45m
```
When the time runs out `termdown` simply exits, so I was combining it with
`terminal-notifier` like this:
```sh
termdown 45m && terminal-notifier -title "Tock" -message "Your tock is over"
```
This is great, but the alert simply disappears after a couple of seconds. What
if I miss it? After all, sometimes when I'm not typing or reading I'm not just
staring at the screen... I wanted the alert to have buttons and stick around
until I explicitly close it, however, `terminal-notifier` doesn't let you do
that. In the README it suggests using
[alerter](https://github.com/vjeantet/alerter) if you want this kind of
functionality. Calling `alerter` with the same arguments as `terminal-notifier`
gives me exactly what I want and now I'm using this:
```sh
termdown 45m && alerter -title "Tock" -message "Your tock is over"
```
This works well, gives me a nice countdown and I'm sure to notice the alert as
it sticks around until I close it. No need to disturb my library neighbors!
