---
title: Developing for Pebble using Docker
tags: pebble, docker
---

A while ago I've come up with an idea for a watch. I thought it would benefit
from custom hardware, but what I really wanted to do is to quickly make a
working prototype and validate it. I looked around for hardware that would be
easy to code for and have all the functions I needed and quickly settled on the
Pebble watch. I found a couple (well, my very generous colleague donated them
to me) and started looking into how to get the toolchain working and put my
ideas to test.

The main Pebble developer support site that comes up now is called [Pebble
Developers](https://developer.rebble.io/developer.pebble.com/index.html). You
can find a lot of helpful content there including [how to get the SDK set up and
running](https://developer.rebble.io/developer.pebble.com/sdk/install/index.html).
I tried following it on my Mac, but just couldn't get it working. Now, this is
exactly the kind of problem that Docker should be able to solve for me! I went
looking for an image that would have the Pebble SDK and found
[andredumas/pebble-deb](https://hub.docker.com/r/andredumas/pebble-dev).

The next thing I had to do was find a way to connect the Pebble to my Mac. I'd
assumed (wrongly) that the USB cable used to charge it would serve that
purpose, but the way to send programs to the watch is via Bluetooth. Reading
[the `pebble` cli tool documentation](https://developer.rebble.io/developer.pebble.com/guides/tools-and-resources/developer-connection/index.html)
reveals that you need to either connect the Pebble directly to your Mac using
Bluetooth at which point it should appear as a device inside `/dev/` or you can
use Developer Connection available through the Android and iOS apps.

I personally would've preferred the former approach as I don't enjoy faffing
about with phone apps when developing, but since I couldn't get the SDK working
on my machine and the Docker is running in a VM (so I couldn't easily pass the
device into the container) I ended up going down the app route. Be warned - the
app is now discontinued and has been removed from the app store, so now you
have to download it from
[APKMirror](https://play.google.com/store/apps/details?id=hu.czandor.pebblerebblehelper).
Not sure what exactly you need to do to get it working with an iPhone, but if
you email me I'll be glad to add instructions for it to this article (or link
to yours).

Once you download the app to your phone you can install it from the APK file.
Launch the app, skip the login and you should have the app working. Next you
need to connect your pebble - click on the refresh icon in the top-right
corner, select the kind of Pebble you have and click on your device in the
list. You then have to go through the Bluetooth pairing procedure after which
the app might decide your watch needs an update. Once that's done and you have
connected your Pebble to the app you'll need to follow the directions for
[turning on Developer Connection](https://developer.rebble.io/developer.pebble.com/guides/tools-and-resources/developer-connection/index.html).

What is truly important here?
- Docker can make things easy!
- (factual) app workflow

What I did:
- plug in to charge
- download Pebble app from APKMirror: https://play.google.com/store/apps/details?id=hu.czandor.pebblerebblehelper
- install from apk
- skip login
- click on the refresh icon (top-right corner)
- select the kind Pebble you have
- click on your device in the list
- check the pairing code and pair
- app can update the watch (photo)
- go through the watch language/permissions/watchface setup
- go into settings, enable developer mode
- go into developer connection, toggle to connect
- connect your phone to a wifi, connect to same wifi with your laptop
- get the example application - https://github.com/pebble-examples/simple-analog/
- pebble build - show using Docker explicitly
- pebble install --phone=[IP shown in the developer connection screen]
- the application is on the watch

Might be nice to explain how you could use Docker running on a Linux machine
and the `--device` flag to skip having to deal with the app completely. I could
do this at the top, run through the basic stuff of building and installing the
example application and then go into how to set up the phone connection in case
you can't get the serial one working.
