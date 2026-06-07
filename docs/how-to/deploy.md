# Deploying The API

This guide describes the deployment currently available at https://labs.musicnark.com/mcn/api, and provides a template for deploying the API to a Debian-based VPS using systemd, Nginx, and Certbot. Replace example domains and paths with values appropriate for your server.

## Prerequisites

- A Debian-based VPS
- A domain or subdomain with A/AAAA records pointing to the VPS
- A sudo-enabled administrative user
- Ports `22`, `80`, and `443` open
- Java, the official Clojure CLI, Git, Nginx, and Certbot installed and available in `PATH`

The deployed service follows this architecture:

```text
Internet
    -> Nginx HTTPS reverse proxy
    -> Clojure/Jetty on port 3000
    -> local EDN cache
```

Port `3000` should not be exposed publicly.

## Service User And Repository

Create an unprivileged user for running the API (we'll call them 'mcn'):

```sh
sudo useradd \
  --system \
  --create-home \
  --home-dir /home/mcn \
  --shell /usr/sbin/nologin \
  mcn
```

Create the application directory and clone the repository:

```sh
sudo mkdir -p /opt/mcn-bike-reviews-search
sudo chown mcn:mcn /opt/mcn-bike-reviews-search
sudo -u mcn git clone \
  https://github.com/musicnark/mcn-bike-reviews-search.git \
  /opt/mcn-bike-reviews-search
```

Run the test suite as the service user:

```sh
sudo -u mcn -H bash -c \
  'cd /opt/mcn-bike-reviews-search && clojure -X:test'
```

## systemd Service

The Clojure CLI writes its dependencies and classpath data to the service user's home directory and the project's `.cpcache` directory. You'll need to create these directories before enabling systemd hardening:

```sh
sudo mkdir -p /home/mcn/.clojure /home/mcn/.m2/repository
sudo mkdir -p /opt/mcn-bike-reviews-search/.cpcache
sudo chown -R mcn:mcn /home/mcn
sudo chown mcn:mcn /opt/mcn-bike-reviews-search/.cpcache
```

Create `/etc/systemd/system/mcn-bike-reviews-search.service`:

```ini
[Unit]
Description=MCN Bike Reviews Search API
After=network.target

[Service]
Type=simple
User=mcn
Group=mcn
WorkingDirectory=/opt/mcn-bike-reviews-search
Environment=PORT=3000
ExecStart=/usr/local/bin/clojure -M:api
Restart=on-failure
RestartSec=5

NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=read-only
ReadWritePaths=/home/mcn /opt/mcn-bike-reviews-search/.cpcache

[Install]
WantedBy=multi-user.target
```

Setting `ProtectHome=true` prevents the Clojure launcher from accessing its dependency cache and can cause a `mkdir: cannot create directory '/home'` error. `ProtectHome=read-only` combined with `ReadWritePaths` keeps the wider filesystem protected while allowing the required cache writes.

Enable and start the service:

```sh
sudo systemctl daemon-reload
sudo systemctl enable --now mcn-bike-reviews-search
sudo systemctl status mcn-bike-reviews-search
```

Verify the API directly:

```sh
curl --fail http://127.0.0.1:3000/api/health
```

## Nginx Reverse Proxy

Create `/etc/nginx/sites-available/mcn-bike-reviews-search`. Replace
`example.com` with the deployment domain:

```nginx
limit_req_zone $binary_remote_addr zone=mcn_api:10m rate=5r/s;

server {
    listen 80;
    listen [::]:80;

    server_name example.com;

    client_max_body_size 64k;

    location = /mcn {
        return 302 /mcn/api;
    }

    location = /mcn/ {
        return 302 /mcn/api;
    }

    location /mcn/ {
        limit_req zone=mcn_api burst=20 nodelay;
        limit_req_status 429;

        proxy_pass http://127.0.0.1:3000/;
        proxy_http_version 1.1;

        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        proxy_connect_timeout 5s;
        proxy_read_timeout 30s;
        proxy_send_timeout 30s;
    }
}
```

The trailing slash in `proxy_pass` removes the public `/mcn/` prefix before
forwarding requests:

```text
/mcn/api/health -> /api/health
```

Enable and validate the configuration:

```sh
sudo ln -s /etc/nginx/sites-available/mcn-bike-reviews-search \
  /etc/nginx/sites-enabled/mcn-bike-reviews-search
sudo nginx -t
sudo systemctl reload nginx
```

## HTTPS

Request a certificate for your domain:

```sh
sudo certbot --nginx -d example.com
```

Verify automatic certificate renewal:

```sh
sudo certbot renew --dry-run
```

## Verification

Check each deployment layer:

```sh
sudo systemctl status mcn-bike-reviews-search
sudo nginx -t
curl --fail http://127.0.0.1:3000/api/health
curl --fail https://example.com/mcn/api/health
```

## Manual Updates

To update and restart the deployment, pull the new source from GitHub and restart the systemd service:

```sh
sudo -u mcn -H bash -c \
  'cd /opt/mcn-bike-reviews-search && git pull --ff-only && clojure -X:test'
sudo systemctl restart mcn-bike-reviews-search
curl --fail https://example.com/mcn/api/health
```

Be sure to run repository commands as `mcn` to avoid changing file ownership.

## Troubleshooting

- **Connection refused:** inspect the systemd logs and confirm port `3000` is
  listening.
- **`mkdir: cannot create directory '/home'`:** confirm `/home/mcn` exists,
  belongs to `mcn`, `ProtectHome` is set to `read-only`, and `/home/mcn` is
  included in `ReadWritePaths`.
- **`route-not-found`:** confirm the requested endpoint begins with `/api` and
  that the Nginx `proxy_pass` URL ends with `/`.
- **`502 Bad Gateway`:** confirm the API service is running and Nginx can reach
  `127.0.0.1:3000`.

Inspect service logs with:

```sh
sudo journalctl -u mcn-bike-reviews-search -n 100 --no-pager
```
