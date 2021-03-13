+++
title = "Backing up saved albums and songs using Spotify API"
draft = true
[taxonomies]
tags = ["spotify"]
+++

Long story short, somebody nicked some authentication tokens for my Spotify
account. It's tied to my Facebook account and I've done all the things I can
think of to kick them off the account - I logged out of all devices via
Spotify, I reauthenticated the Spotify app via Facebook and updated my Facebook
credentials to include 2FA. And yet, they're able to authenticate to my account
on their devices and play music. I'm assuming the reason is that old FB tokens
are kept on Spotify servers and people are able to authenticate using those
as there is no mechanism to invalidate them on the Spotify side. Time for a new
account it is! I'd like to keep all my saved albums and songs though. I found
some applications that let you export your playlists, but nothing that would
work with albums and individual songs. I'm sure it's somewhere out there, but
I've been itching to use the Spotify API and this seems like a perfect excuse!

I decided this will be a little JS application so I don't need to deal with
any backend code (and have an easier time hosting it simply using GitHub Pages).
Since I've been working with Vue recently and it makes it easy to start and
build a small application without much investment I thought I'd go with that.

The user would get to the page / application, be asked to authenticate with
both the account they want to copy from and the account that's on the receiving
end and then click a button asking for their albums and songs to be copied. I'd
have a couple of progress bars to indicate stuff is happening and also a box
where the application could output some messages to the user explaining what
it's up to currently. I'm not great with UI, but using Bootstrap I managed to
put this together:

<img src="/images/spotify-backup/app-layout.png" class="screenshot">


