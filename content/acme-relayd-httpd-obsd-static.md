# OpenBSD: Setup static sites with acme-client, httpd and relayd
## Introduction

I wanted to create an article site to share my knowledge and also contribute to OpenBSD somehow, so I had the idea to host it in an OpenBSD VPS. Searching how can I do it, I found about `httpd`, `relayd` and `acme-client`, all available at [OpenBSD man pages](https://man.openbsd.org).
## How it works?

We will setup `httpd(8)` for serving the content, `relayd(8)` for tls termination and security headers, `acme-client(1)` for managing and getting certifications, and `pf` for setting rules in our server.
```
| request   acme-client (Let's Encrypt cert renewals)
|                       ^
|                       |
v                       v
---> relayd (tls termination + security headers) ---> httpd (serves static files)
```

## acme-client

If you want, copy the example `acme-client.conf(5)` in  `/etc/examples/acme-client.conf` to `/etc/acme-client.conf` to have it as template.
```sh
# cp /etc/examples/acme-client.conf /etc/
```
Now, configure `acme-client.conf(5)` in `/etc` directory, example:
```
# Authorities contactable by acme
authority letsencrypt {
	api url "https://acme-v02.api.letsencrypt.org/directory"
	account key "/etc/acme/letsencrypt-privkey.pem"
}

# Certificates for domain via letsencrypt
domain fugu.cafe {
    # Subdomains
	alternative names { www.fugu.cafe }
	domain key "/etc/ssl/private/fugu.cafe.key"
	domain full chain certificate "/etc/ssl/fugu.cafe.crt"
	sign with letsencrypt
}

```
Now you can generate the certificate. If you already set up relayd, you must reload it after generating the certificate.
```sh
# acme-client -v fugu.cafe
```
If you want renewal, you can setup a `cron(8)` job with this command plus `relayd(8)` reloading. 
```
# Every 03:00
0 3 * * * acme-client fugu.cafe && rcctl restart relayd
```

## httpd

If you want, copy the example `httpd.conf(5)` in `/etc/examples/httpd.conf` to `/etc/httpd.conf` to have it as template.
```sh
# cp /etc/examples/httpd.conf /etc/
```
Now, configure `httpd.conf(5)` in `/etc` directory. It's recommended to put your static site under `/var/www/htdocs/`. Here is an example:
```
# Redirects to https
server "fugu.cafe" {
    listen on 127.0.0.1 port 80

    location * {
        block return 301 "https://$HTTP_HOST$REQUEST_URI"
    }
}

server "fugu.cafe" {
    alias "www.fugu.cafe"

    listen on 127.0.0.1 port 8080

    location "/.well-known/acme-challenge/*" {
       root "/acme"
       request strip 2
    }

    # Recommended to set it in htdocs, i did it outside.
    # e.g. /htdocs/fugu.cafe
    root "/fugu.cafe"

    location "/" {
        block return 302 "/main/"
    }

    # Block hidden files
    location "/.*" {
        block
    }
}

```
It's recommended to change your static website directory owner to `www`, because `httpd(8)` is run as `www` user as default.
```sh
# chown -R www:www /var/www/fugu.cafe
```
Check for any errors in httpd configuration.
```sh
# httpd -n 
```
If your configuration is OK, you can start or restart `httpd(8)`.
```sh
# rcctl enable httpd
# rcctl start httpd
```

## relayd

If you want, copy the example `relayd.conf(5)` in  `/etc/examples/relayd.conf` to `/etc/relayd.conf` to have it as template.
```sh
# cp /etc/examples/relayd.conf /etc/
```
Now, configure `relayd.conf(5)` in `/etc` directory, example:
```
ipv4="<address4>"
ipv6="<address6>" 

table <local> { 127.0.0.1 }

http protocol https {
  tls keypair "fugu.cafe"

  tls ciphers "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256"

  # Security headers
  match response header set "Referrer-Policy" value "same-origin"
  match response header set "X-Frame-Options" value "deny"
  match response header set "X-XSS-Protection" value "1; mode=block"
  match response header set "X-Content-Type-Options" value "nosniff"
  match response header set "Strict-Transport-Security" value "max-age=31536000; includeSubDomains; preload"
  match response header set "Content-Security-Policy" value "default-src 'self'; style-src 'self'; img-src 'self'; base-uri 'self'; frame-ancestors"
  match response header set "Permissions-Policy" value "accelerometer=()"
  match response header set "Cache-Control" value "max-age=86400"
  match request header append "X-Forwarded-For" value "$REMOTE_ADDR"
  match request header append "X-Forwarded-Port" value "$REMOTE_PORT"
 
  return error
  pass
}

relay wwwtls {
  listen on $ipv4 port 443 tls
  protocol https
  forward to <local> port 8080
}

relay www6tls {
  listen on $ipv6 port 443 tls
  protocol https
  forward to <local> port 8080
}

# I didn't want to go direct to httpd in case of http for usability
relay www {
  listen on $ipv4 port 80
  forward to <local> port 80
}

relay www6 {
  listen on $ipv6 port 80
  forward to <local> port 80
}

```
Feel free to set security headers to your need. Also, check for any errors.
```sh
# relayd -n 
```
If your configuration is OK, you can start or restart `relayd(8)`.
```sh
# rcctl enable relayd
# rcctl start relayd
```

## Conclusion

If everything is OK, your static site is now fully served by OpenBSD, using `httpd(8)` for content, `relayd(8)` for tls termination and headers, and `acme-client(1)` for certificate management. It’s a simple, secure and minimal setup, entirely maintained within the system itself.
