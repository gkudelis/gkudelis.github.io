---
title: Log monitor using socat, rtail, nginx and supervisor
tags: logging, monitoring, socat, rtail, supervisor, nginx
published: 2015-12-03
---

### Intro

Working as a developer I find myself spending a significant amount of time
debugging software. And for debugging web backend software the web server logs
can be an incredibly useful tool. For a while I've been monitoring my server
logs using something like `tail -F /var/log/(something)/error_log` on the
server. However, this is not ideal as I often end up with a bunch of terminal
tabs monitoring different logs and swearing every time my connection drops and
ssh decides to time out. This could be solved with a combination of `autossh`
and `tmux`, but I wanted to be able to search the logs and filter using regular
expressions. Even more importantly, I wanted to share the power of logs with
my colleagues who might not be as enthusiastic about a bunch of terminal tools.

After finding `rtail` I realised it covers most of my requirements. It has a
client which takes input from STDIN and sends it via network to the `rtail`
server. The server collects the log streams coming from the different clients
and serves all of them via HTTP. It allows users to switch between the streams,
filter using regex and some other useful stuff.

My problem with using strainght `rtail` was the lack of security. The authors
say you should use it behind a VPN, but that's not something I wanted to do.
Instead, I decided I'm going to place it behind an `nginx` reverse proxy to
handle authentication and use `socat` to wrap the UDP packages into an SSL
connection as the logs travel between my servers. I ended up using `supervisor`
to make all of these into services that are started at boot and restarted in
case of crashes.

### Setting up `socat`

The first thing to get working was `socat`. Once I know the connection works I
should be able to get `rtail` using it. There's a guide that I followed for
creating an SSL connection: [Securing Traffic Between two Socat Instances Using
SSL](http://www.dest-unreach.org/socat/doc/socat-openssltunnel.html). The only
difference was that I did not want to use SSL for client authentication. That
allows me to skip having to generate certificates for every client.

First step is generating the server key and certificate:
```
openssl genrsa -out rtail-server.key 1024
```
Next create a self-signed certificate:
```
openssl req -new -key rtail-server.key -x509 -days 3653 -out rtail-server.crt
```
You can safely ignore all the prompts by hitting enter. The next step is
generating the .pem file:
```
cat rtail-server.key rtail-server.crt >rtail-server.pem
```
The .key and .pem files must remain secret. It's a good idea to make them only
readable by you:
```
chmod 600 rtail-server.key rtail-server.pem
```
The .pem file is used on the `rtail` server and the .crt is used by every
`rtail` client. Make sure you use something secure (like `scp`) when moving
the .pem file to the server!

Now we can set up the SSL connection between the two servers and make sure
everything works. In my case it did not, turns out there was a firewall
blocking the port I wanted to use. After sorting out the firewall everything
went smoothly. For the sake of argument let's say that we'll be using port
5333 for the SSL connection.

First you have to set up the server end of the SSL connection:
```
socat OPENSSL-LISTEN:5333,fork,reuseaddr,cert=/path/to/srv-rtail.pem,verify=0 STDIO
```
Here we're telling `socat` we want it to fork for every new client (which means
we can send multiple streams to it at the same time), to authenticate itself
using the .pem file we generated previously and to not authenticate clients.
We connect the stream to STDIO for debugging purposes, but later it will be
pointing at the local `rtail` server. On the client side we run the client version:
```
socat STDIO OPENSSL:my.rtail.server.com:5333,cafile=/path/to/srv-rtail.crt
```
We're making `socat` listen to the STDIO on the client side (which will later
be listening to the output of `rtail` client applications on that server) and
stream it to our `rtail` server via SSL. It will be using the .crt file to
authenticate the server. If everything is correct you should be able to type
things in on the client side and see them turn up on the server side.

### Setting up `rtail`

Now we can get `rtail` using the SSL connection. If `rtail` had an option to
use STDIO we could just pipe it to/from `socat`. However, `rtail` clients
communicate with the server via UDP packets - we need to make `socat` listen to
these packets on the client end and reproduce them on the server end. The only
change to the above `socat` setup we replace STDIO with the corresponding UDP
setup. Let's say we're using the port 5700 for `rtail` UDP communication. On
the server this becomes:
```
socat OPENSSL-LISTEN:5333,fork,reuseaddr,cert=/path/to/srv-rtail.pem,verify=0 UDP:localhost:5700
```
And on the client side:
```
socat UDP-LISTEN:5700 OPENSSL:my.rtail.server.com:5333,cafile=/path/to/srv-rtail.crt
```
To set up `rtail` we need to run the client pointing it to the 5333 UDP port:
```
rtail --port 5700 --id my-stream-name
```
and on the server:
```
rtail-server --web-port 5700 --udp-port 5700
```
The id on the client side simply helps identify the stream on the web interface. 
At this point if you point your browser to the port 5700 of the `rtail` server
you should see the `rtail` web interface. Test by typing things into the input
stream of the rtail client - it should show up in the web interface under the
stream called "my-stream-name". If you can't see the web interface or nothing
comes through double check the firewall settings.

You can now pipe your log files into the `rtail` client from `tail`:
```
tail -F /some/log/file | rtail --port 5700 --id some-log-file-stream
```

### Seting up `supervisor`

The first time setting this up I just left all the `rtail` and `socat` instances
running in my `tmux` sessions on various servers. Needless to say that's not
sustainable. The easiest solution I've found was to set up `supervisor` to
manage these processes.

On the client side we need to run the `socat` instance and as many `rtail`
clients as we need. For one client the configuration would look something like
this:
```
[program:rtail-client-socat-relay]
command = socat UDP-LISTEN:5700 OPENSSL:my.rtail.server.com:5333,cafile=/path/to/srv-rtail.crt
autostart = true
autorestart = true
user = nobody
priority = 900

[program:some-stream-rtail]
command = /bin/bash -c "tail -F /some/stream/log | /usr/bin/rtail --port 5700 --id some-stream --mute"
killasgroup = true
autostart = true
autorestart = true
user = some_stream_owner
```
Since `socat` needs to authenticate the server make sure the user `nobody` has
read access to the certificate. In addition to that, the `some_stream_owner`
user must have read access to the log file. The priority setting defines the
order in which the processes should be started with the lower-numbered ones
being started first. The default is 999, so by setting the `socat` connection
priority to 900 we make it start before `rtail`.

On the server side we open the SSL connection and start the `rtail` server:
```
[program:rtail-server]
command = /usr/local/bin/rtail-server --web-port 5700 --udp-port 5700
autostart = true
autorestart = true
user = nobody

[program:rtail-server-socat-relay]
command = /usr/bin/socat OPENSSL-LISTEN:5333,fork,reuseaddr,cert=/path/to/srv-rtail.pem,verify=0 UDP:localhost:5700
autostart = true
autorestart = true
user = cert_owner
```
The `cert_owner` must have read access to the certificate. The `fork` flag
for `socat` makes it fork when new connection request is received, create
a new connection and forward the content to the same UDP port `rtail` is
listening on. The `verify=0` flag tells `socat` that the client certificates
don't need to be verified.

If this does not work first make sure running the same commands manually works,
then check permissions and have a look at the log file produced by `supervisor`.
It contains details of which processes have started successfully and can help
debug the configuration.

### Setting up `nginx`

The last part is setting up `nginx` as a reverse proxy with simple HTTP
authentication. We tell `nginx` about the `rtail` web service we have running,
then ask for all requests coming to that domain to be redirected to the rtail
service subject to authentication. Here's a good manual on how to generate the
auth file: [How To Set Up HTTP Authentication With Nginx On Ubuntu 12.10](https://www.digitalocean.com/community/tutorials/how-to-set-up-http-authentication-with-nginx-on-ubuntu-12-10).
Below is the configuration I ended up with. There is nothing but the auth file
in the `/avr/www/mydomain.com/rtail/` folder.

```
upstream rtail {
    server 127.0.0.1:5700;
}

server {
    server_name rtail.mydomain.com;

    auth_basic "My rtail service";
    auth_basic_user_file /var/www/mydomain.com/rtail/auth;

    location / {
        proxy_pass http://rtail;
        proxy_redirect off;
    }

}
```

### Final Remarks
One thing I'd like to see (and quite possibly get involved in myself) is
creating an option that allows the `rtail` client to send its data using
standard output. There's really no need to use the UDP stack if we're just
piping the data through `socat` anyway. On the server side the `socat`
instances could dump their data into a named pipe that the `rtail` server would
be listening to. I think this would be a cleaner solution.

