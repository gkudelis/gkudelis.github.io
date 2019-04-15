---
title: Developing for Pebble using Docker
tags: pebble, docker
---



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
- the application is on the phone!

Might be nice to explain how you could use Docker running on a Linux machine
and the `--device` flag to skip having to deal with the app completely. I could
do this at the top, run through the basic stuff of building and installing the
example application and then go into how to set up the phone connection in case
you can't get the serial one working.
