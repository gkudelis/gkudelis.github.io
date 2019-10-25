+++
title = "Developing for Pebble using Docker"
[taxonomies]
tags = ["pebble", "docker"]
+++

A while ago I've come up with an idea for a watch. I thought it would benefit
from custom hardware, but what I really wanted to do is to quickly make a
working prototype and validate it. I looked around for hardware that would be
easy to code for and have all the functions I needed and quickly settled on the
Pebble watch. I found a couple (well, my very generous colleague donated them
to me) and started looking into how to get the toolchain working and put my
ideas to test.

### Connecting to the Pebble

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
[the pebble cli tool documentation](https://developer.rebble.io/developer.pebble.com/guides/tools-and-resources/developer-connection/index.html)
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

### Building and installing an app

Now that the Developer connection is on let's take an example app and install
it on the watch. A nice example is [the simple-analog
watchface](https://github.com/pebble-examples/simple-analog/). To clone it run
```sh
git clone https://github.com/pebble-examples/simple-analog.git
```
Enter the project folder, then build the app:
```sh
docker run --rm -it -v $PWD:/pebble andredumas/pebble-dev build
```
If all went well (I get some Python errors, but the build still gets completed)
take the IP shown in Developer Connection and install the app on your Pebble:
```sh
docker run --rm -it -v $PWD:/pebble andredumas/pebble-dev install --phone=192.168.0.1
```
This will install the watchface on your Pebble and it should become the current
watchface. You can follow exactly the same process to build and install apps.

### Alternative connection options

I have to admit that I don't love having to download and install an app from a
mirror just to use the phone as a WiFi-Bluetooth bridge. I tried setting up
a Bluetooth serial connection to the Pebble from a Linux machine (hoping I can
use the `--device` option to `docker run` to pass it through to the container),
but it just kept disconnecting and I gave up. If you know how to get it
working - please let me know!
